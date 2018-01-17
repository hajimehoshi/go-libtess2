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

// #include "geom.h"
// #include "mesh.h"
// #include "priorityq.h"
// #include "sweep.h"
// #include "tess.h"
// #include "tesselator.h"
//
// static ActiveRegion* allocActiveRegion(TESStesselator* tess) {
//   return (ActiveRegion*)bucketAlloc(tess->regionPool);
// }
//
// void AddWinding(TESShalfEdge* eDst, TESShalfEdge* eSrc);
// void SpliceMergeVertices( TESStesselator *tess, TESShalfEdge *e1, TESShalfEdge *e2 );
// void DeleteRegion( TESStesselator *tess, ActiveRegion *reg );
// void ConnectLeftDegenerate( TESStesselator *tess, ActiveRegion *regUp, TESSvertex *vEvent );
// void AddRightEdges( TESStesselator *tess, ActiveRegion *regUp, TESShalfEdge *eFirst, TESShalfEdge *eLast, TESShalfEdge *eTopLeft, int cleanUp );
// void ConnectLeftDegenerate( TESStesselator *tess, ActiveRegion *regUp, TESSvertex *vEvent );
// void ConnectLeftVertex( TESStesselator *tess, TESSvertex *vEvent );
// void ConnectRightVertex( TESStesselator *tess, ActiveRegion *regUp, TESShalfEdge *eBottomLeft );
// TESShalfEdge *FinishLeftRegions( TESStesselator *tess, ActiveRegion *regFirst, ActiveRegion *regLast );
// ActiveRegion *TopLeftRegion( TESStesselator *tess, ActiveRegion *reg );
import "C"

func assert(cond bool) {
	if !cond {
		panic("libtess2: assertion error")
	}
}

func max(a, b C.int) C.int {
	if a < b {
		return b
	}
	return a
}

func dst(e *C.TESShalfEdge) *C.TESSvertex {
	return e.Sym.Org
}

func regionBelow(r *C.ActiveRegion) *C.ActiveRegion {
	return dictKey(dictPred(r.nodeUp))
}

func regionAbove(r *C.ActiveRegion) *C.ActiveRegion  {
	return dictKey(dictSucc(r.nodeUp))
}

func adjust(x C.TESSreal) C.TESSreal {
	if x > 0 {
		return x
	}
	return 0.01
}

//export SweepEvent
//
// sweepEvent does everything necessary when the sweep line crosses a vertex.
// Updates the mesh and the edge dictionary.
func SweepEvent(tess *C.TESStesselator, vEvent *C.TESSvertex) {
	tess.event = vEvent /* for access in EdgeLeq() */

	// Check if this vertex is the right endpoint of an edge that is
	// already in the dictionary.  In this case we don't need to waste
	// time searching for the location to insert new edges.
	e := vEvent.anEdge
	for e.activeRegion == nil {
		e = e.Onext
		if e == vEvent.anEdge {
			// All edges go right -- not incident to any processed edges
			C.ConnectLeftVertex(tess, vEvent)
			return
		}
	}

	// Processing consists of two phases: first we "finish" all the
	// active regions where both the upper and lower edges terminate
	// at vEvent (ie. vEvent is closing off these regions).
	// We mark these faces "inside" or "outside" the polygon according
	// to their winding number, and delete the edges from the dictionary.
	// This takes care of all the left-going edges from vEvent.
	regUp := C.TopLeftRegion(tess, e.activeRegion)
	reg := regionBelow(regUp)
	eTopLeft := reg.eUp
	eBottomLeft := C.FinishLeftRegions(tess, reg, nil)

	// Next we process all the right-going edges from vEvent.  This
	// involves adding the edges to the dictionary, and creating the
	// associated "active regions" which record information about the
	// regions between adjacent dictionary edges.
	if eBottomLeft.Onext == eTopLeft {
		// No right-going edges -- add a temporary "fixable" edge
		C.ConnectRightVertex(tess, regUp, eBottomLeft)
	} else {
		C.AddRightEdges(tess, regUp, eBottomLeft.Onext, eTopLeft, eTopLeft, 1)
	}
}

// addSentinel makes the sentinel coordinates big enough that they will never be
// merged with real input features.
//
// We add two sentinel edges above and below all other edges,
// to avoid special cases at the top and bottom.
func addSentinel(tess *C.TESStesselator, smin, smax C.TESSreal, t C.TESSreal) {
	reg := C.allocActiveRegion(tess)

	e := C.tessMeshMakeEdge(tess.mesh)

	e.Org.s = smax
	e.Org.t = t
	dst(e).s = smin
	dst(e).t = t
	tess.event = dst(e)

	reg.eUp = e
	reg.sentinel = 1
	reg.nodeUp = dictInsert(tess.dict, reg)
}

// initEdgeDict:
// We maintain an ordering of edge intersections with the sweep line.
// This order is maintained in a dynamic dictionary.
func initEdgeDict(tess *C.TESStesselator) {
	tess.dict = dictNewDict(tess)

	w := (tess.bmax[0] - tess.bmin[0])
	h := (tess.bmax[1] - tess.bmin[1])

	// If the bbox is empty, ensure that sentinels are not coincident by
	// slightly enlarging it.
	smin := tess.bmin[0] - adjust(w)
	smax := tess.bmax[0] + adjust(w)
	tmin := tess.bmin[1] - adjust(h)
	tmax := tess.bmax[1] + adjust(h)

	addSentinel(tess, smin, smax, tmin)
	addSentinel(tess, smin, smax, tmax)
}

func doneEdgeDict(tess *C.TESStesselator) {
	fixedEdges := 0
	for {
		reg := C.dictKey(C.dictMin(tess.dict))
		if reg == nil {
			break
		}
		// At the end of all processing, the dictionary should contain
		// only the two sentinel edges, plus at most one "fixable" edge
		// created by ConnectRightVertex().
		if reg.sentinel == 0 {
			assert(reg.fixUpperEdge != 0)
			fixedEdges++
			assert(fixedEdges == 1)
		}
		assert(reg.windingNumber == 0)
		C.DeleteRegion(tess, reg)
		// tessMeshDelete( reg.eUp );
	}
	C.dictDeleteDict(tess.dict)
}

// removeDegenerateEdges removes zero-length edges, and contours with fewer than 3 vertices.
func removeDegenerateEdges(tess *C.TESStesselator) {
	eHead := &tess.mesh.eHead
	var eNext *C.TESShalfEdge
	for e := eHead.next; e != eHead; e = eNext {
		eNext = e.next
		eLnext := e.Lnext

		if C.VertEq(e.Org, dst(e)) != 0 && e.Lnext.Lnext != e {
			// Zero-length edge, contour has at least 3 edges
			C.SpliceMergeVertices(tess, eLnext, e) /* deletes e.Org */
			C.tessMeshDelete(tess.mesh, e)
			e = eLnext
			eLnext = e.Lnext
		}
		if eLnext.Lnext == e {
			// Degenerate contour (one or two edges)
			if eLnext != e {
				if eLnext == eNext || eLnext == eNext.Sym {
					eNext = eNext.next
				}
				C.tessMeshDelete(tess.mesh, eLnext)
			}
			if e == eNext || e == eNext.Sym {
				eNext = eNext.next
			}
			C.tessMeshDelete(tess.mesh, e)
		}
	}
}

// initPriorityQ inserts all vertices into the priority queue which determines the
// order in which vertices cross the sweep line.
func initPriorityQ(tess *C.TESStesselator) {
	vertexCount := C.int(0)

	vHead := &tess.mesh.vHead
	for v := vHead.next; v != vHead; v = v.next {
		vertexCount++
	}
	// Make sure there is enough space for sentinels.
	vertexCount += max(8, tess.alloc.extraVertices)

	tess.pq = pqNewPriorityQ(vertexCount)

	vHead = &tess.mesh.vHead
	for v := vHead.next; v != vHead; v = v.next {
		v.pqHandle = pqInsert(tess.pq, v)
	}
}

func donePriorityQ(tess *C.TESStesselator) {
	pqDeletePriorityQ(tess.pq)
}

// removeDegenerateFaces deletes any degenerate faces with only two edges.  WalkDirtyRegions()
// will catch almost all of these, but it won't catch degenerate faces
// produced by splice operations on already-processed edges.
// The two places this can happen are in FinishLeftRegions(), when
// we splice in a "temporary" edge produced by ConnectRightVertex(),
// and in CheckForLeftSplice(), where we splice already-processed
// edges to ensure that our dictionary invariants are not violated
// by numerical errors.
//
// In both these cases it is *very* dangerous to delete the offending
// edge at the time, since one of the routines further up the stack
// will sometimes be keeping a pointer to that edge.
func removeDegenerateFaces(tess *C.TESStesselator, mesh *C.TESSmesh) bool {
	var fNext *C.TESSface
	for f := mesh.fHead.next; f != &mesh.fHead; f = fNext {
		fNext = f.next
		e := f.anEdge
		assert(e.Lnext != e)

		if e.Lnext.Lnext == e {
			// A face with only two edges
			C.AddWinding(e.Onext, e)
			if C.tessMeshDelete(tess.mesh, e) == 0 {
				return false
			}
		}
	}
	return true
}

//export tessComputeInterior
//
// tessComputeInterior( tess ) computes the planar arrangement specified
// by the given contours, and further subdivides this arrangement
// into regions.  Each region is marked "inside" if it belongs
// to the polygon, according to the rule given by tess.windingRule.
// Each interior region is guaranteed be monotone.
func tessComputeInterior(tess *C.TESStesselator) C.int {
	//TESSvertex *v, *vNext;

	// Each vertex defines an event for our sweep line.  Start by inserting
	// all the vertices in a priority queue.  Events are processed in
	// lexicographic order, ie.
	//
	//	e1 < e2  iff  e1.x < e2.x || (e1.x == e2.x && e1.y < e2.y)
	removeDegenerateEdges(tess)
	initPriorityQ(tess)
	initEdgeDict(tess)

	for {
		v := (*C.TESSvertex)(C.pqExtractMin(tess.pq))
		if v == nil {
			break
		}
		for {
			vNext := (*C.TESSvertex)(C.pqMinimum(tess.pq))
			if vNext == nil || C.VertEq(vNext, v) == 0 {
				break
			}

			// Merge together all vertices at exactly the same location.
			// This is more efficient than processing them one at a time,
			// simplifies the code (see ConnectLeftDegenerate), and is also
			// important for correct handling of certain degenerate cases.
			// For example, suppose there are two identical edges A and B
			// that belong to different contours (so without this code they would
			// be processed by separate sweep events).  Suppose another edge C
			// crosses A and B from above.  When A is processed, we split it
			// at its intersection point with C.  However this also splits C,
			// so when we insert B we may compute a slightly different
			// intersection point.  This might leave two edges with a small
			// gap between them.  This kind of error is especially obvious
			// when using boundary extraction (TESS_BOUNDARY_ONLY).
			vNext = (*C.TESSvertex)(C.pqExtractMin(tess.pq))
			C.SpliceMergeVertices(tess, v.anEdge, vNext.anEdge)
		}
		SweepEvent(tess, v)
	}

	// Set tess.event for debugging purposes
	tess.event = dictKey(dictMin(tess.dict)).eUp.Org
	doneEdgeDict(tess)
	donePriorityQ(tess)

	if !removeDegenerateFaces(tess, tess.mesh) {
		return 0
	}
	C.tessMeshCheckMesh(tess.mesh)

	return 1
}
