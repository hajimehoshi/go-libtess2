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
// void AddRightEdges( TESStesselator *tess, ActiveRegion *regUp, TESShalfEdge *eFirst, TESShalfEdge *eLast, TESShalfEdge *eTopLeft, int cleanUp );
// TESShalfEdge *FinishLeftRegions( TESStesselator *tess, ActiveRegion *regFirst, ActiveRegion *regLast );
// ActiveRegion *TopLeftRegion( TESStesselator *tess, ActiveRegion *reg );
// void ComputeWinding( TESStesselator *tess, ActiveRegion *reg );
// int FixUpperEdge( TESStesselator *tess, ActiveRegion *reg, TESShalfEdge *newEdge );
// ActiveRegion *AddRegionBelow( TESStesselator *tess, ActiveRegion *regAbove, TESShalfEdge *eNewUp );
// ActiveRegion *TopRightRegion( ActiveRegion *reg );
// int CheckForIntersect( TESStesselator *tess, ActiveRegion *regUp );
// int CheckForLeftSplice( TESStesselator *tess, ActiveRegion *regUp );
// int CheckForRightSplice( TESStesselator *tess, ActiveRegion *regUp );
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

func rPrev(e *C.TESShalfEdge) *C.TESShalfEdge {
	return e.Sym.Onext
}

func oPrev(e *C.TESShalfEdge) *C.TESShalfEdge {
	return e.Sym.Lnext
}

func lPrev(e *C.TESShalfEdge) *C.TESShalfEdge {
	return e.Onext.Sym
}

func dNext(e *C.TESShalfEdge) *C.TESShalfEdge {
	return rPrev(e).Sym
}

func regionBelow(r *C.ActiveRegion) *C.ActiveRegion {
	return dictKey(dictPred(r.nodeUp))
}

func regionAbove(r *C.ActiveRegion) *C.ActiveRegion {
	return dictKey(dictSucc(r.nodeUp))
}

func adjust(x C.TESSreal) C.TESSreal {
	if x > 0 {
		return x
	}
	return 0.01
}

//export WalkDirtyRegions
//
// walkDirtyRegions:
// When the upper or lower edge of any region changes, the region is
// marked "dirty".  This routine walks through all the dirty regions
// and makes sure that the dictionary invariants are satisfied
// (see the comments at the beginning of this file).  Of course
// new dirty regions can be created as we make changes to restore
// the invariants.
func WalkDirtyRegions(tess *C.TESStesselator, regUp *C.ActiveRegion) {
	regLo := regionBelow(regUp)

	for {
		// Find the lowest dirty region (we walk from the bottom up).
		for regLo.dirty != 0 {
			regUp = regLo
			regLo = regionBelow(regLo)
		}
		if regUp.dirty == 0 {
			regLo = regUp
			regUp = regionAbove(regUp)
			if regUp == nil || regUp.dirty == 0 {
				// We've walked all the dirty regions
				return
			}
		}
		regUp.dirty = 0 /* false */
		eUp := regUp.eUp
		eLo := regLo.eUp

		if dst(eUp) != dst(eLo) {
			// Check that the edge ordering is obeyed at the Dst vertices.
			if C.CheckForLeftSplice(tess, regUp) != 0 {

				// If the upper or lower edge was marked fixUpperEdge, then
				// we no longer need it (since these edges are needed only for
				// vertices which otherwise have no right-going edges).
				if regLo.fixUpperEdge != 0 {
					C.DeleteRegion(tess, regLo)
					C.tessMeshDelete(tess.mesh, eLo)
					regLo = regionBelow(regUp)
					eLo = regLo.eUp
				} else if regUp.fixUpperEdge != 0 {
					C.DeleteRegion(tess, regUp)
					C.tessMeshDelete(tess.mesh, eUp)
					regUp = regionAbove(regLo)
					eUp = regUp.eUp
				}
			}
		}
		if eUp.Org != eLo.Org {
			if dst(eUp) != dst(eLo) && regUp.fixUpperEdge == 0 && regLo.fixUpperEdge == 0 && (dst(eUp) == tess.event || dst(eLo) == tess.event) {
				// When all else fails in CheckForIntersect(), it uses tess.event
				// as the intersection location.  To make this possible, it requires
				// that tess.event lie between the upper and lower edges, and also
				// that neither of these is marked fixUpperEdge (since in the worst
				// case it might splice one of these edges into tess.event, and
				// violate the invariant that fixable edges are the only right-going
				// edge from their associated vertex).
				if C.CheckForIntersect(tess, regUp) != 0 {
					// WalkDirtyRegions() was called recursively; we're done
					return
				}
			} else {
				// Even though we can't use CheckForIntersect(), the Org vertices
				// may violate the dictionary edge ordering.  Check and correct this.
				C.CheckForRightSplice(tess, regUp)
			}
		}
		if eUp.Org == eLo.Org && dst(eUp) == dst(eLo) {
			// A degenerate loop consisting of only two edges -- delete it.
			C.AddWinding(eLo, eUp)
			C.DeleteRegion(tess, regUp)
			C.tessMeshDelete(tess.mesh, eUp)
			regUp = regionAbove(regLo)
		}
	}
}

// connectRightVertex:
// Purpose: connect a "right" vertex vEvent (one where all edges go left)
// to the unprocessed portion of the mesh.  Since there are no right-going
// edges, two regions (one above vEvent and one below) are being merged
// into one.  "regUp" is the upper of these two regions.
//
// There are two reasons for doing this (adding a right-going edge):
//  - if the two regions being merged are "inside", we must add an edge
//    to keep them separated (the combined region would not be monotone).
//  - in any case, we must leave some record of vEvent in the dictionary,
//    so that we can merge vEvent with features that we have not seen yet.
//    For example, maybe there is a vertical edge which passes just to
//    the right of vEvent; we would like to splice vEvent into this edge.
//
// However, we don't want to connect vEvent to just any vertex.  We don''t
// want the new edge to cross any other edges; otherwise we will create
// intersection vertices even when the input data had no self-intersections.
// (This is a bad thing; if the user's input data has no intersections,
// we don't want to generate any false intersections ourselves.)
//
// Our eventual goal is to connect vEvent to the leftmost unprocessed
// vertex of the combined region (the union of regUp and regLo).
// But because of unseen vertices with all right-going edges, and also
// new vertices which may be created by edge intersections, we don''t
// know where that leftmost unprocessed vertex is.  In the meantime, we
// connect vEvent to the closest vertex of either chain, and mark the region
// as "fixUpperEdge".  This flag says to delete and reconnect this edge
// to the next processed vertex on the boundary of the combined region.
// Quite possibly the vertex we connected to will turn out to be the
// closest one, in which case we won''t need to make any changes.
func connectRightVertex(tess *C.TESStesselator, regUp *C.ActiveRegion, eBottomLeft *C.TESShalfEdge) {
	eTopLeft := eBottomLeft.Onext
	regLo := regionBelow(regUp)
	eUp := regUp.eUp
	eLo := regLo.eUp
	degenerate := false

	if dst(eUp) != dst(eLo) {
		C.CheckForIntersect(tess, regUp)
	}

	// Possible new degeneracies: upper or lower edge of regUp may pass
	// through vEvent, or may coincide with new intersection vertex
	if C.VertEq(eUp.Org, tess.event) != 0 {
		C.tessMeshSplice(tess.mesh, oPrev(eTopLeft), eUp)
		regUp = C.TopLeftRegion(tess, regUp)
		eTopLeft = regionBelow(regUp).eUp
		C.FinishLeftRegions(tess, regionBelow(regUp), regLo)
		degenerate = true
	}
	if C.VertEq(eLo.Org, tess.event) != 0 {
		C.tessMeshSplice(tess.mesh, eBottomLeft, oPrev(eLo))
		eBottomLeft = C.FinishLeftRegions(tess, regLo, nil)
		degenerate = true
	}
	if degenerate {
		C.AddRightEdges(tess, regUp, eBottomLeft.Onext, eTopLeft, eTopLeft, 1 /* true */)
		return
	}

	// Non-degenerate situation -- need to add a temporary, fixable edge.
	// Connect to the closer of eLo.Org, eUp.Org.
	var eNew *C.TESShalfEdge
	if C.VertLeq(eLo.Org, eUp.Org) != 0 {
		eNew = oPrev(eLo)
	} else {
		eNew = eUp
	}
	eNew = C.tessMeshConnect(tess.mesh, lPrev(eBottomLeft), eNew)

	// Prevent cleanup, otherwise eNew might disappear before we've even
	// had a chance to mark it as a temporary edge.
	C.AddRightEdges(tess, regUp, eNew, eNew.Onext, eNew.Onext, 0 /* false */)
	eNew.Sym.activeRegion.fixUpperEdge = 1 /* true */
	WalkDirtyRegions(tess, regUp)
}

// connectLeftDegenerate:
// The event vertex lies exacty on an already-processed edge or vertex.
// Adding the new vertex involves splicing it into the already-processed
// part of the mesh.
func connectLeftDegenerate(tess *C.TESStesselator, regUp *C.ActiveRegion, vEvent *C.TESSvertex) {
	// Because vertices at exactly the same location are merged together
	// before we process the sweep event, some degenerate cases can't occur.
	// However if someone eventually makes the modifications required to
	// merge features which are close together, the cases below marked
	// TOLERANCE_NONZERO will be useful.  They were debugged before the
	// code to merge identical vertices in the main loop was added.
	const TOLERANCE_NONZERO = false

	e := regUp.eUp
	if C.VertEq(e.Org, vEvent) != 0 {
		/* e.Org is an unprocessed vertex - just combine them, and wait
		* for e.Org to be pulled from the queue
		 */
		assert(TOLERANCE_NONZERO)
		C.SpliceMergeVertices(tess, e, vEvent.anEdge)
		return
	}

	if C.VertEq(dst(e), vEvent) == 0 {
		/* General case -- splice vEvent into edge e which passes through it */
		C.tessMeshSplitEdge(tess.mesh, e.Sym)
		if regUp.fixUpperEdge != 0 {
			/* This edge was fixable -- delete unused portion of original edge */
			C.tessMeshDelete(tess.mesh, e.Onext)
			regUp.fixUpperEdge = 0 // false
		}
		C.tessMeshSplice(tess.mesh, vEvent.anEdge, e)
		// recurse
		sweepEvent(tess, vEvent)
		return
	}

	// vEvent coincides with e.Dst, which has already been processed.
	// Splice in the additional right-going edges.
	assert(TOLERANCE_NONZERO)
	regUp = C.TopRightRegion(regUp)
	reg := regionBelow(regUp)
	eTopRight := reg.eUp.Sym
	eTopLeft := eTopRight.Onext
	eLast := eTopRight.Onext
	if reg.fixUpperEdge != 0 {
		// Here e.Dst has only a single fixable edge going right.
		// We can delete it since now we have some real right-going edges.
		assert(eTopLeft != eTopRight) // there are some left edges too
		C.DeleteRegion(tess, reg)
		C.tessMeshDelete(tess.mesh, eTopRight)
		eTopRight = oPrev(eTopLeft)
	}
	C.tessMeshSplice(tess.mesh, vEvent.anEdge, eTopRight)
	if C.EdgeGoesLeft(eTopLeft) == 0 {
		// e.Dst had no left-going edges -- indicate this to AddRightEdges()
		eTopLeft = nil
	}
	C.AddRightEdges(tess, regUp, eTopRight.Onext, eLast, eTopLeft, 1 /* true */)
}

// connectLeftVertex:
// Purpose: connect a "left" vertex (one where both edges go right)
// to the processed portion of the mesh.  Let R be the active region
// containing vEvent, and let U and L be the upper and lower edge
// chains of R.  There are two possibilities:
//
// - the normal case: split R into two regions, by connecting vEvent to
//   the rightmost vertex of U or L lying to the left of the sweep line
//
// - the degenerate case: if vEvent is close enough to U or L, we
//   merge vEvent into that edge chain.  The subcases are:
//	- merging with the rightmost vertex of U or L
//	- merging with the active edge of U or L
//	- merging with an already-processed portion of U or L
func connectLeftVertex(tess *C.TESStesselator, vEvent *C.TESSvertex) {
	var tmp C.ActiveRegion

	// assert( vEvent.anEdge.Onext.Onext == vEvent.anEdge );

	// Get a pointer to the active region containing vEvent
	tmp.eUp = vEvent.anEdge.Sym
	regUp := dictKey(dictSearch(tess.dict, &tmp))
	regLo := regionBelow(regUp)
	if regLo == nil {
		// This may happen if the input polygon is coplanar.
		return
	}
	eUp := regUp.eUp
	eLo := regLo.eUp

	// Try merging with U or L first
	if C.tesedgeSign(dst(eUp), vEvent, eUp.Org) == 0 {
		connectLeftDegenerate(tess, regUp, vEvent)
		return
	}

	// Connect vEvent to rightmost processed vertex of either chain.
	// e.Dst is the vertex that we will connect to vEvent.
	var reg *C.ActiveRegion
	if C.VertLeq(dst(eLo), dst(eUp)) != 0 {
		reg = regUp
	} else {
		reg = regLo
	}

	if regUp.inside != 0 || reg.fixUpperEdge != 0 {
		var eNew *C.TESShalfEdge
		if reg == regUp {
			eNew = C.tessMeshConnect(tess.mesh, vEvent.anEdge.Sym, eUp.Lnext)
		} else {
			tempHalfEdge := C.tessMeshConnect(tess.mesh, dNext(eLo), vEvent.anEdge)
			eNew = tempHalfEdge.Sym
		}
		if reg.fixUpperEdge != 0 {
			C.FixUpperEdge(tess, reg, eNew)
		} else {
			C.ComputeWinding(tess, C.AddRegionBelow(tess, regUp, eNew))
		}
		sweepEvent(tess, vEvent)
	} else {
		// The new vertex is in a region which does not belong to the polygon.
		// We don''t need to connect this vertex to the rest of the mesh.
		C.AddRightEdges(tess, regUp, vEvent.anEdge, vEvent.anEdge, nil, 1)
	}
}

// sweepEvent does everything necessary when the sweep line crosses a vertex.
// Updates the mesh and the edge dictionary.
func sweepEvent(tess *C.TESStesselator, vEvent *C.TESSvertex) {
	tess.event = vEvent // for access in EdgeLeq()

	// Check if this vertex is the right endpoint of an edge that is
	// already in the dictionary.  In this case we don't need to waste
	// time searching for the location to insert new edges.
	e := vEvent.anEdge
	for e.activeRegion == nil {
		e = e.Onext
		if e == vEvent.anEdge {
			// All edges go right -- not incident to any processed edges
			connectLeftVertex(tess, vEvent)
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
		connectRightVertex(tess, regUp, eBottomLeft)
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
		sweepEvent(tess, v)
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
