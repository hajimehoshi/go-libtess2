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
import "C"

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
						C.tessMeshDelete(mesh, eSym)
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
