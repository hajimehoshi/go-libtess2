// SGI FREE SOFTWARE LICENSE B (Version 2.0, Sept. 18, 2008)
// Copyright (C) [dates of first publication] Silicon Graphics, Inc.
// All Rights Reserved.
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
// of the Software, and to permit persons to whom the Software is furnished to do so,
// subject to the following conditions:
//
// The above copyright notice including the dates of first publication and either this
// permission notice or a reference to http://oss.sgi.com/projects/FreeB/ shall be
// included in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
// INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
// PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL SILICON GRAPHICS, INC.
// BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
// TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE
// OR OTHER DEALINGS IN THE SOFTWARE.
//
// Except as contained in this notice, the name of Silicon Graphics, Inc. shall not
// be used in advertising or otherwise to promote the sale, use or other dealings in
// this Software without prior written authorization from Silicon Graphics, Inc.

package libtess2

// #include "mesh.h"
// #include "bucketalloc.h"
//
// #include <stdlib.h>
//
// static void* golibtess2_stdAlloc(void* userData, unsigned int size) {
//   int* allocated = ( int*)userData;
//   TESS_NOTUSED(userData);
//   *allocated += (int)size;
//   return malloc(size);
// }
import "C"

import (
	"unsafe"
)

// tessMeshDelete removes the edge eDel.  There are several cases:
// if (eDel.Lface != eDel.Rface), we join two loops into one; the loop
// eDel.Lface is deleted.  Otherwise, we are splitting one loop into two;
// the newly created loop will contain eDel.Dst.  If the deletion of eDel
// would create isolated vertices, those are deleted as well.
//
// This function could be implemented as two calls to tessMeshSplice
// plus a few calls to memFree, but this would allocate and delete
// unnecessary vertices and faces.
func tessMeshDelete(mesh *C.TESSmesh, eDel *C.TESShalfEdge) {
	eDelSym := eDel.Sym
	joiningLoops := false

	// First step: disconnect the origin vertex eDel.Org.  We make all
	// changes to get a consistent mesh in this "intermediate" state.
	if eDel.Lface != rFace(eDel) {
		// We are joining two loops into one -- remove the left face
		joiningLoops = true
		C.KillFace(mesh, eDel.Lface, rFace(eDel))
	}

	if eDel.Onext == eDel {
		C.KillVertex(mesh, eDel.Org, nil)
	} else {
		// Make sure that eDel.Org and eDel.Rface point to valid half-edges
		rFace(eDel).anEdge = oPrev(eDel)
		eDel.Org.anEdge = eDel.Onext

		C.Splice(eDel, oPrev(eDel))
		if !joiningLoops {
			newFace := (*C.TESSface)(C.bucketAlloc(mesh.faceBucket))

			// We are splitting one loop into two -- create a new loop for eDel.
			C.MakeFace(newFace, eDel, eDel.Lface)
		}
	}

	// Claim: the mesh is now in a consistent state, except that eDel.Org
	// may have been deleted.  Now we disconnect eDel.Dst.
	if eDelSym.Onext == eDelSym {
		C.KillVertex(mesh, eDelSym.Org, nil)
		C.KillFace(mesh, eDelSym.Lface, nil)
	} else {
		// Make sure that eDel.Dst and eDel.Lface point to valid half-edges
		eDel.Lface.anEdge = oPrev(eDelSym)
		eDelSym.Org.anEdge = eDelSym.Onext
		C.Splice(eDelSym, oPrev(eDelSym))
	}

	// Any isolated vertices or faces have already been freed.
	C.KillEdge(mesh, eDel)
}

// All these routines can be implemented with the basic edge
// operations above.  They are provided for convenience and efficiency.

// tessMeshAddEdgeVertex creates a new edge eNew such that
// eNew == eOrg.Lnext, and eNew.Dst is a newly created vertex.
// eOrg and eNew will have the same left face.
func tessMeshAddEdgeVertex(mesh *C.TESSmesh, eOrg *C.TESShalfEdge) *C.TESShalfEdge {
	eNew := C.MakeEdge(mesh, eOrg)
	eNewSym := eNew.Sym

	// Connect the new edge appropriately
	C.Splice(eNew, eOrg.Lnext)

	// Set the vertex and face information
	eNew.Org = dst(eOrg)
	newVertex := (*C.TESSvertex)(C.bucketAlloc(mesh.vertexBucket))
	C.MakeVertex(newVertex, eNewSym, eNew.Org)
	eNew.Lface = eOrg.Lface
	eNewSym.Lface = eOrg.Lface

	return eNew
}

// tessMeshSplitEdge splits eOrg into two edges eOrg and eNew,
// such that eNew == eOrg.Lnext.  The new vertex is eOrg.Dst == eNew.Org.
// eOrg and eNew will have the same left face.
func tessMeshSplitEdge(mesh *C.TESSmesh, eOrg *C.TESShalfEdge) *C.TESShalfEdge {
	tempHalfEdge := tessMeshAddEdgeVertex(mesh, eOrg)

	eNew := tempHalfEdge.Sym

	// Disconnect eOrg from eOrg.Dst and connect it to eNew.Org
	C.Splice(eOrg.Sym, oPrev(eOrg.Sym))
	C.Splice(eOrg.Sym, eNew)

	// Set the vertex and face information
	setDst(eOrg, eNew.Org)
	dst(eNew).anEdge = eNew.Sym // may have pointed to eOrg.Sym
	setRFace(eNew, rFace(eOrg))
	eNew.winding = eOrg.winding // copy old winding information
	eNew.Sym.winding = eOrg.Sym.winding

	return eNew
}

// tessMeshConnect creates a new edge from eOrg.Dst
// to eDst.Org, and returns the corresponding half-edge eNew.
// If eOrg.Lface == eDst.Lface, this splits one loop into two,
// and the newly created loop is eNew.Lface.  Otherwise, two disjoint
// loops are merged into one, and the loop eDst.Lface is destroyed.
//
// If (eOrg == eDst), the new face will have only two edges.
// If (eOrg.Lnext == eDst), the old face is reduced to a single edge.
// If (eOrg.Lnext.Lnext == eDst), the old face is reduced to two edges.
func tessMeshConnect(mesh *C.TESSmesh, eOrg *C.TESShalfEdge, eDst *C.TESShalfEdge) *C.TESShalfEdge {
	joiningLoops := false
	eNew := C.MakeEdge(mesh, eOrg)
	eNewSym := eNew.Sym

	if eDst.Lface != eOrg.Lface {
		// We are connecting two disjoint loops -- destroy eDst.Lface
		joiningLoops = true
		C.KillFace(mesh, eDst.Lface, eOrg.Lface)
	}

	// Connect the new edge appropriately
	C.Splice(eNew, eOrg.Lnext)
	C.Splice(eNewSym, eDst)

	// Set the vertex and face information
	eNew.Org = dst(eOrg)
	eNewSym.Org = eDst.Org
	eNew.Lface = eOrg.Lface
	eNewSym.Lface = eOrg.Lface

	// Make sure the old face points to a valid half-edge
	eOrg.Lface.anEdge = eNewSym

	if !joiningLoops {
		newFace := (*C.TESSface)(C.bucketAlloc(mesh.faceBucket))

		// We split one loop into two -- the new loop is eNew.Lface
		C.MakeFace(newFace, eNew, eOrg.Lface)
	}
	return eNew
}

// tessMeshZapFace destroys a face and removes it from the
// global face list.  All edges of fZap will have a nil pointer as their
// left face.  Any edges which also have a nil pointer as their right face
// are deleted entirely (along with any isolated vertices this produces).
// An entire mesh can be deleted by zapping its faces, one at a time,
// in any order.  Zapped faces cannot be used in further mesh operations!
func tessMeshZapFace(mesh *C.TESSmesh, fZap *C.TESSface) {
	eStart := fZap.anEdge

	// walk around face, deleting edges whose right face is also nil
	eNext := eStart.Lnext
	for {
		e := eNext
		eNext = e.Lnext

		e.Lface = nil
		if rFace(e) == nil {
			// delete the edge -- see TESSmeshDelete above

			if e.Onext == e {
				C.KillVertex(mesh, e.Org, nil)
			} else {
				// Make sure that e.Org points to a valid half-edge
				e.Org.anEdge = e.Onext
				C.Splice(e, oPrev(e))
			}
			eSym := e.Sym
			if eSym.Onext == eSym {
				C.KillVertex(mesh, eSym.Org, nil)
			} else {
				// Make sure that eSym.Org points to a valid half-edge
				eSym.Org.anEdge = eSym.Onext
				C.Splice(eSym, oPrev(eSym))
			}
			C.KillEdge(mesh, e)
		}
		if e == eStart {
			break
		}
	}

	// delete from circular doubly-linked list
	fPrev := fZap.prev
	fNext := fZap.next
	fNext.prev = fPrev
	fPrev.next = fNext

	C.bucketFree(mesh.faceBucket, unsafe.Pointer(fZap))
}

// tessMeshNewMesh creates a new mesh with no edges, no vertices,
// and no loops (what we usually call a "face").
func tessMeshNewMesh(alloc *C.TESSalloc) *C.TESSmesh {
	mesh := (*C.TESSmesh)(C.golibtess2_stdAlloc(alloc.userData, C.sizeof_struct_TESSmesh))

	if alloc.meshEdgeBucketSize < 16 {
		alloc.meshEdgeBucketSize = 16
	}
	if alloc.meshEdgeBucketSize > 4096 {
		alloc.meshEdgeBucketSize = 4096
	}

	if alloc.meshVertexBucketSize < 16 {
		alloc.meshVertexBucketSize = 16
	}
	if alloc.meshVertexBucketSize > 4096 {
		alloc.meshVertexBucketSize = 4096
	}

	if alloc.meshFaceBucketSize < 16 {
		alloc.meshFaceBucketSize = 16
	}
	if alloc.meshFaceBucketSize > 4096 {
		alloc.meshFaceBucketSize = 4096
	}

	mesh.edgeBucket = C.createBucketAlloc(alloc, C.CString("Mesh Edges"), C.sizeof_EdgePair, C.uint(alloc.meshEdgeBucketSize))
	mesh.vertexBucket = C.createBucketAlloc(alloc, C.CString("Mesh Vertices"), C.sizeof_struct_TESSvertex, C.uint(alloc.meshVertexBucketSize))
	mesh.faceBucket = C.createBucketAlloc(alloc, C.CString("Mesh Faces"), C.sizeof_struct_TESSface, C.uint(alloc.meshFaceBucketSize))

	v := &mesh.vHead
	f := &mesh.fHead
	e := &mesh.eHead
	eSym := &mesh.eHeadSym

	v.next = v
	v.prev = v
	v.anEdge = nil

	f.next = f
	f.prev = f
	f.anEdge = nil
	f.trail = nil
	f.marked = 0 /* false */
	f.inside = 0 /* false */

	e.next = e
	e.Sym = eSym
	e.Onext = nil
	e.Lnext = nil
	e.Org = nil
	e.Lface = nil
	e.winding = 0
	e.activeRegion = nil

	eSym.next = eSym
	eSym.Sym = e
	eSym.Onext = nil
	eSym.Lnext = nil
	eSym.Org = nil
	eSym.Lface = nil
	eSym.winding = 0
	eSym.activeRegion = nil

	return mesh
}

// tessMeshUnion forms the union of all structures in
// both meshes, and returns the new mesh (the old meshes are destroyed).
//
// TODO: This function is not used anywhere?
func tessMeshUnion(alloc *C.TESSalloc, mesh1, mesh2 *C.TESSmesh) *C.TESSmesh {
	f1 := &mesh1.fHead
	v1 := &mesh1.vHead
	e1 := &mesh1.eHead
	f2 := &mesh2.fHead
	v2 := &mesh2.vHead
	e2 := &mesh2.eHead

	// Add the faces, vertices, and edges of mesh2 to those of mesh1
	if f2.next != f2 {
		f1.prev.next = f2.next
		f2.next.prev = f1.prev
		f2.prev.next = f1
		f1.prev = f2.prev
	}

	if v2.next != v2 {
		v1.prev.next = v2.next
		v2.next.prev = v1.prev
		v2.prev.next = v1
		v1.prev = v2.prev
	}

	if e2.next != e2 {
		e1.Sym.next.Sym.next = e2.next
		e2.next.Sym.next = e1.Sym.next
		e2.Sym.next.Sym.next = e1
		e1.Sym.next = e2.Sym.next
	}
	return mesh1
}

func countFaceVerts(f *C.TESSface) int {
	eCur := f.anEdge
	n := 0
	for {
		n++
		eCur = eCur.Lnext
		if eCur == f.anEdge {
			break
		}
	}
	return n
}

func tessMeshMergeConvexFaces(mesh *C.TESSmesh, maxVertsPerFace int) {
	for f := mesh.fHead.next; f != &mesh.fHead; f = f.next {
		// Skip faces which are outside the result.
		if f.inside == 0 /* false */ {
			continue
		}

		eCur := f.anEdge
		vStart := eCur.Org

		for {
			eNext := eCur.Lnext
			eSym := eCur.Sym

			// Try to merge if the neighbour face is valid.
			if eSym != nil && eSym.Lface != nil && eSym.Lface.inside != 0 /* true */ {
				// Try to merge the neighbour faces if the resulting polygons
				// does not exceed maximum number of vertices.
				curNv := countFaceVerts(f)
				symNv := countFaceVerts(eSym.Lface)
				if curNv+symNv-2 <= maxVertsPerFace {
					// Merge if the resulting poly is convex.
					if tesvertCCW(lPrev(eCur).Org, eCur.Org, eSym.Lnext.Lnext.Org) && tesvertCCW(lPrev(eSym).Org, eSym.Org, eCur.Lnext.Lnext.Org) {
						eNext = eSym.Lnext
						tessMeshDelete(mesh, eSym)
						eCur = nil
					}
				}
			}

			if eCur != nil && eCur.Lnext.Org == vStart {
				break
			}

			// Continue to next edge.
			eCur = eNext
		}
	}
}

// tessMeshCheckMesh checks a mesh for self-consistency.
func tessMeshCheckMesh(mesh *C.TESSmesh) {
	fHead := &mesh.fHead
	vHead := &mesh.vHead
	eHead := &mesh.eHead

	var f *C.TESSface
	fPrev := fHead
	for {
		f = fPrev.next
		if f == fHead {
			break
		}
		assert(f.prev == fPrev)
		e := f.anEdge
		for {
			assert(e.Sym != e)
			assert(e.Sym.Sym == e)
			assert(e.Lnext.Onext.Sym == e)
			assert(e.Onext.Sym.Lnext == e)
			assert(e.Lface == f)
			e = e.Lnext
			if e == f.anEdge {
				break
			}
		}
		fPrev = f
	}
	assert(f.prev == fPrev && f.anEdge == nil)

	var v *C.TESSvertex
	vPrev := vHead
	for {
		v = vPrev.next
		if v == vHead {
			break
		}
		assert(v.prev == vPrev)
		e := v.anEdge
		for {
			assert(e.Sym != e)
			assert(e.Sym.Sym == e)
			assert(e.Lnext.Onext.Sym == e)
			assert(e.Onext.Sym.Lnext == e)
			assert(e.Org == v)
			e = e.Onext
			if e == v.anEdge {
				break
			}
		}
		vPrev = v
	}
	assert(v.prev == vPrev && v.anEdge == nil)

	var e *C.TESShalfEdge
	ePrev := eHead
	for {
		e = ePrev.next
		if e == eHead {
			break
		}
		assert(e.Sym.next == ePrev.Sym)
		assert(e.Sym != e)
		assert(e.Sym.Sym == e)
		assert(e.Org != nil)
		assert(dst(e) != nil)
		assert(e.Lnext.Onext.Sym == e)
		assert(e.Onext.Sym.Lnext == e)
		ePrev = e
	}
	assert(e.Sym.next == ePrev.Sym)
	assert(e.Sym == &mesh.eHeadSym)
	assert(e.Sym.Sym == e)
	assert(e.Org == nil && dst(e) == nil)
	assert(e.Lface == nil && rFace(e) == nil)
}
