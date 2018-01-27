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

// #include "tesselator.h"
import "C"

// Invariants for the Edge Dictionary.
// - each pair of adjacent edges e2=Succ(e1) satisfies EdgeLeq(e1,e2)
//   at any valid location of the sweep event
// - if EdgeLeq(e2,e1) as well (at any valid sweep event), then e1 and e2
//   share a common endpoint
// - for each e, e->Dst has been processed, but not e->Org
// - each edge e satisfies vertLeq(e.Dst,event) && vertLeq(event,e.Org)
//   where "event" is the current sweep line event.
// - no edge e has zero length
//
// Invariants for the Mesh (the processed portion).
// - the portion of the mesh left of the sweep line is a planar graph,
//   ie. there is //some// way to embed it in the plane
// - no processed edge has zero length
// - no two processed vertices have identical coordinates
// - each "inside" region is monotone, ie. can be broken into two chains
//   of monotonically increasing vertices according to VertLeq(v1,v2)
//   - a non-invariant: these chains may intersect (very slightly)
//
// Invariants for the Sweep.
// - if none of the edges incident to the event vertex have an activeRegion
//   (ie. none of these edges are in the edge dictionary), then the vertex
//   has only right-going edges.
// - if an edge is marked "fixUpperEdge" (it is a temporary edge introduced
//   by ConnectRightVertex), then it is the only right-going edge from
//   its associated vertex.  (This says that these edges exist only
//   when it is necessary.)

const (
	undef = index(^0)
)

func assert(cond bool) {
	if !cond {
		panic("libtess2: assertion error")
	}
}

func min(a, b C.int) C.int {
	if a < b {
		return a
	}
	return b
}

func minf(a, b float) float {
	if a < b {
		return a
	}
	return b
}

func max(a, b C.int) C.int {
	if a < b {
		return b
	}
	return a
}

func maxf(a, b float) float {
	if a < b {
		return b
	}
	return a
}

func rFace(e *halfEdge) *face {
	return e.Sym.Lface
}

func setRFace(e *halfEdge, f *face) {
	e.Sym.Lface = f
}

func dst(e *halfEdge) *vertex {
	return e.Sym.Org
}

func setDst(e *halfEdge, v *vertex) {
	e.Sym.Org = v
}

func rPrev(e *halfEdge) *halfEdge {
	return e.Sym.Onext
}

func oPrev(e *halfEdge) *halfEdge {
	return e.Sym.Lnext
}

func lPrev(e *halfEdge) *halfEdge {
	return e.Onext.Sym
}

func dNext(e *halfEdge) *halfEdge {
	return rPrev(e).Sym
}

func regionBelow(r *activeRegion) *activeRegion {
	return dictKey(dictPred((*dictNode)(r.nodeUp)))
}

func regionAbove(r *activeRegion) *activeRegion {
	return dictKey(dictSucc((*dictNode)(r.nodeUp)))
}

func adjust(x float) float {
	if x > 0 {
		return x
	}
	return 0.01
}

func deleteRegion(tess *tesselator, reg *activeRegion) {
	if reg.fixUpperEdge {
		// It was created with zero winding number, so it better be
		// deleted with zero winding number (ie. it better not get merged
		// with a real edge).
		assert(reg.eUp.winding == 0)
	}
	reg.eUp.activeRegion = nil
	dictDelete((*dictNode)(reg.nodeUp))
}

// edgeLeq:
// Both edges must be directed from right to left (this is the canonical
// direction for the upper edge of each region).
//
// The strategy is to evaluate a "t" value for each edge at the
// current sweep line position, given by tess.event.  The calculations
// are designed to be very stable, but of course they are not perfect.
//
// Special case: if both edge destinations are at the sweep event,
// we sort the edges by slope (they would otherwise compare equally).
func edgeLeq(tess *tesselator, reg1 *activeRegion, reg2 *activeRegion) bool {
	event := tess.event
	e1 := reg1.eUp
	e2 := reg2.eUp

	if dst(e1) == event {
		if dst(e2) == event {
			// Two edges right of the sweep line which meet at the sweep event.
			// Sort them by slope.
			if vertLeq(e1.Org, e2.Org) {
				return tesedgeSign(dst(e2), e1.Org, e2.Org) <= 0
			}
			return tesedgeSign(dst(e1), e2.Org, e1.Org) >= 0
		}
		return tesedgeSign(dst(e2), event, e2.Org) <= 0
	}
	if dst(e2) == event {
		return tesedgeSign(dst(e1), event, e1.Org) >= 0
	}

	// General case - compute signed distance *from* e1, e2 to event
	t1 := tesedgeEval(dst(e1), event, e1.Org)
	t2 := tesedgeEval(dst(e2), event, e2.Org)
	return (t1 >= t2)
}

// addWinding:
// When we merge two edges into one, we need to compute the combined
// winding of the new edge.
func addWinding(eDst *halfEdge, eSrc *halfEdge) {
	eDst.winding += eSrc.winding
	eDst.Sym.winding += eSrc.Sym.winding
}

// fixUpperEdge replace an upper edge which needs fixing (see ConnectRightVertex).
func fixUpperEdge(tess *tesselator, reg *activeRegion, newEdge *halfEdge) {
	assert(reg.fixUpperEdge)
	tessMeshDelete(tess.mesh, reg.eUp)
	reg.fixUpperEdge = false
	reg.eUp = newEdge
	newEdge.activeRegion = reg
}

func topLeftRegion(tess *tesselator, reg *activeRegion) *activeRegion {
	org := reg.eUp.Org

	// Find the region above the uppermost edge with the same origin
	for {
		reg = regionAbove(reg)
		if reg.eUp.Org != org {
			break
		}
	}

	// If the edge above was a temporary edge introduced by ConnectRightVertex,
	// now is the time to fix it.
	if reg.fixUpperEdge {
		e := tessMeshConnect(tess.mesh, regionBelow(reg).eUp.Sym, reg.eUp.Lnext)
		fixUpperEdge(tess, reg, e)
		reg = regionAbove(reg)
	}
	return reg
}

func topRightRegion(reg *activeRegion) *activeRegion {
	d := dst(reg.eUp)
	// Find the region above the uppermost edge with the same destination
	for {
		reg = regionAbove(reg)
		if dst(reg.eUp) != d {
			break
		}
	}
	return reg
}

// addRegionBelow adds a new active region to the sweep line, *somewhere* below "regAbove"
// (according to where the new edge belongs in the sweep-line dictionary).
// The upper edge of the new region will be "eNewUp".
// Winding number and "inside" flag are not updated.
func addRegionBelow(tess *tesselator, regAbove *activeRegion, eNewUp *halfEdge) *activeRegion {
	regNew := &activeRegion{}
	regNew.eUp = eNewUp
	regNew.nodeUp = dictInsertBefore((*dict)(tess.dict), (*dictNode)(regAbove.nodeUp), regNew)
	eNewUp.activeRegion = regNew
	return regNew
}

func isWindingInside(tess *tesselator, n int) bool {
	switch tess.windingRule {
	case windingRuleOdd:
		return (n & 1) != 0
	case windingRuleNonzero:
		return (n != 0)
	case windingRulePositive:
		return (n > 0)
	case windingRuleNegative:
		return (n < 0)
	case windingRuleAbsGeqTwo:
		return (n >= 2) || (n <= -2)
	}
	panic("not reached")
}

func computeWinding(tess *tesselator, reg *activeRegion) {
	reg.windingNumber = regionAbove(reg).windingNumber + int(reg.eUp.winding)
	reg.inside = isWindingInside(tess, int(reg.windingNumber))
}

// finishRegion deletes a region from the sweep line.  This happens when the upper
// and lower chains of a region meet (at a vertex on the sweep line).
// The "inside" flag is copied to the appropriate mesh face (we could
// not do this before -- since the structure of the mesh is always
// changing, this face may not have even existed until now).
func finishRegion(tess *tesselator, reg *activeRegion) {
	e := reg.eUp
	f := e.Lface

	f.inside = reg.inside
	f.anEdge = e // optimization for tessMeshTessellateMonoRegion()
	deleteRegion(tess, reg)
}

// finishLeftRegions:
// We are given a vertex with one or more left-going edges.  All affected
// edges should be in the edge dictionary.  Starting at regFirst.eUp,
// we walk down deleting all regions where both edges have the same
// origin vOrg.  At the same time we copy the "inside" flag from the
// active region to the face, since at this point each face will belong
// to at most one region (this was not necessarily true until this point
// in the sweep).  The walk stops at the region above regLast; if regLast
// is NULL we walk as far as possible.  At the same time we relink the
// mesh if necessary, so that the ordering of edges around vOrg is the
// same as in the dictionary.
func finishLeftRegions(tess *tesselator, regFirst *activeRegion, regLast *activeRegion) *halfEdge {
	regPrev := regFirst
	ePrev := regFirst.eUp
	for regPrev != regLast {
		regPrev.fixUpperEdge = false // placement was OK
		reg := regionBelow(regPrev)
		e := reg.eUp
		if e.Org != ePrev.Org {
			if !reg.fixUpperEdge {
				// Remove the last left-going edge.  Even though there are no further
				// edges in the dictionary with this origin, there may be further
				// such edges in the mesh (if we are adding left edges to a vertex
				// that has already been processed).  Thus it is important to call
				// FinishRegion rather than just deleteRegion.
				finishRegion(tess, regPrev)
				break
			}
			// If the edge below was a temporary edge introduced by
			// ConnectRightVertex, now is the time to fix it.
			e = tessMeshConnect(tess.mesh, lPrev(ePrev), e.Sym)
			fixUpperEdge(tess, reg, e)
		}

		// Relink edges so that ePrev.Onext == e
		if ePrev.Onext != e {
			tessMeshSplice(tess.mesh, oPrev(e), e)
			tessMeshSplice(tess.mesh, ePrev, e)
		}
		finishRegion(tess, regPrev) // may change reg.eUp
		ePrev = reg.eUp
		regPrev = reg
	}
	return ePrev
}

// addRightEdges:
// Purpose: insert right-going edges into the edge dictionary, and update
// winding numbers and mesh connectivity appropriately.  All right-going
// edges share a common origin vOrg.  Edges are inserted CCW starting at
// eFirst; the last edge inserted is eLast.Oprev.  If vOrg has any
// left-going edges already processed, then eTopLeft must be the edge
// such that an imaginary upward vertical segment from vOrg would be
// contained between eTopLeft.Oprev and eTopLeft; otherwise eTopLeft
// should be nil.
func addRightEdges(tess *tesselator, regUp *activeRegion, eFirst *halfEdge, eLast *halfEdge, eTopLeft *halfEdge, cleanUp bool) {
	firstTime := true

	// Insert the new right-going edges in the dictionary
	e := eFirst
	for {
		assert(vertLeq(e.Org, dst(e)))
		addRegionBelow(tess, regUp, e.Sym)
		e = e.Onext
		if e == eLast {
			break
		}
	}

	// Walk *all* right-going edges from e.Org, in the dictionary order,
	// updating the winding numbers of each region, and re-linking the mesh
	// edges to match the dictionary ordering (if necessary).
	if eTopLeft == nil {
		eTopLeft = rPrev(regionBelow(regUp).eUp)
	}
	regPrev := regUp
	ePrev := eTopLeft
	var reg *activeRegion
	for {
		reg = regionBelow(regPrev)
		e = reg.eUp.Sym
		if e.Org != ePrev.Org {
			break
		}

		if e.Onext != ePrev {
			// Unlink e from its current position, and relink below ePrev
			tessMeshSplice(tess.mesh, oPrev(e), e)
			tessMeshSplice(tess.mesh, oPrev(ePrev), e)
		}
		// Compute the winding number and "inside" flag for the new regions
		reg.windingNumber = regPrev.windingNumber - int(e.winding)
		reg.inside = isWindingInside(tess, int(reg.windingNumber))

		// Check for two outgoing edges with same slope -- process these
		// before any intersection tests (see example in tessComputeInterior).
		regPrev.dirty = true
		if !firstTime && checkForRightSplice(tess, regPrev) {
			addWinding(e, ePrev)
			deleteRegion(tess, regPrev)
			tessMeshDelete(tess.mesh, ePrev)
		}
		firstTime = false
		regPrev = reg
		ePrev = e
	}
	regPrev.dirty = true
	assert(regPrev.windingNumber-int(e.winding) == reg.windingNumber)

	if cleanUp {
		// Check for intersections between newly adjacent edges.
		walkDirtyRegions(tess, regPrev)
	}
}

// spliceMergeVertices:
// Two vertices with idential coordinates are combined into one.
// e1.Org is kept, while e2.Org is discarded.
func spliceMergeVertices(tess *tesselator, e1 *halfEdge, e2 *halfEdge) {
	tessMeshSplice(tess.mesh, e1, e2)
}

// vertexWeights finds some weights which describe how the intersection vertex is
// a linear combination of "org" and "dest".  Each of the two edges
// which generated "isect" is allocated 50% of the weight; each edge
// splits the weight between its org and dst according to the
// relative distance to "isect".
func vertexWeights(isect *vertex, org *vertex, dst *vertex) {
	t1 := vertL1dist(org, isect)
	t2 := vertL1dist(dst, isect)

	w0 := 0.5 * t2 / (t1 + t2)
	w1 := 0.5 * t1 / (t1 + t2)
	isect.coords[0] += w0*org.coords[0] + w1*dst.coords[0]
	isect.coords[1] += w0*org.coords[1] + w1*dst.coords[1]
	isect.coords[2] += w0*org.coords[2] + w1*dst.coords[2]
}

// getIntersectData:
// We've computed a new intersection point, now we need a "data" pointer
// from the user so that we can refer to this new vertex in the
// rendering callbacks.
func getIntersectData(tess *tesselator, isect *vertex,
	orgUp *vertex, dstUp *vertex,
	orgLo *vertex, dstLo *vertex) {
	isect.coords[0] = 0
	isect.coords[1] = 0
	isect.coords[2] = 0
	isect.idx = undef
	vertexWeights(isect, orgUp, dstUp)
	vertexWeights(isect, orgLo, dstLo)
}

// checkForRightSplice checks the upper and lower edge of "regUp", to make sure that the
// eUp.Org is above eLo, or eLo.Org is below eUp (depending on which
// origin is leftmost).
//
// The main purpose is to splice right-going edges with the same
// dest vertex and nearly identical slopes (ie. we can't distinguish
// the slopes numerically).  However the splicing can also help us
// to recover from numerical errors.  For example, suppose at one
// point we checked eUp and eLo, and decided that eUp.Org is barely
// above eLo.  Then later, we split eLo into two edges (eg. from
// a splice operation like this one).  This can change the result of
// our test so that now eUp.Org is incident to eLo, or barely below it.
// We must correct this condition to maintain the dictionary invariants.
//
// One possibility is to check these edges for intersection again
// (ie. CheckForIntersect).  This is what we do if possible.  However
// CheckForIntersect requires that tess.event lies between eUp and eLo,
// so that it has something to fall back on when the intersection
// calculation gives us an unusable answer.  So, for those cases where
// we can't check for intersection, this routine fixes the problem
// by just splicing the offending vertex into the other edge.
// This is a guaranteed solution, no matter how degenerate things get.
// Basically this is a combinatorial solution to a numerical problem.
func checkForRightSplice(tess *tesselator, regUp *activeRegion) bool {
	regLo := regionBelow(regUp)
	eUp := regUp.eUp
	eLo := regLo.eUp

	if vertLeq(eUp.Org, eLo.Org) {
		if tesedgeSign(dst(eLo), eUp.Org, eLo.Org) > 0 {
			return false
		}

		// eUp.Org appears to be below eLo
		if !vertEq(eUp.Org, eLo.Org) {
			// Splice eUp.Org into eLo
			tessMeshSplitEdge(tess.mesh, eLo.Sym)
			tessMeshSplice(tess.mesh, eUp, oPrev(eLo))
			regUp.dirty = true
			regLo.dirty = true

		} else if eUp.Org != eLo.Org {
			// merge the two vertices, discarding eUp.Org
			pqDelete((*pq)(tess.pq), eUp.Org.pqHandle)
			spliceMergeVertices(tess, oPrev(eLo), eUp)
		}
	} else {
		if tesedgeSign(dst(eUp), eLo.Org, eUp.Org) < 0 {
			return false
		}

		// eLo.Org appears to be above eUp, so splice eLo.Org into eUp
		regionAbove(regUp).dirty = true
		regUp.dirty = true
		tessMeshSplitEdge(tess.mesh, eUp.Sym)
		tessMeshSplice(tess.mesh, oPrev(eLo), eUp)
	}
	return true
}

// checkForLeftSplice checks the upper and lower edge of "regUp", to make sure that the
// eUp.Dst is above eLo, or eLo.Dst is below eUp (depending on which
// destination is rightmost).
//
// Theoretically, this should always be true.  However, splitting an edge
// into two pieces can change the results of previous tests.  For example,
// suppose at one point we checked eUp and eLo, and decided that eUp.Dst
// is barely above eLo.  Then later, we split eLo into two edges (eg. from
// a splice operation like this one).  This can change the result of
// the test so that now eUp.Dst is incident to eLo, or barely below it.
// We must correct this condition to maintain the dictionary invariants
// (otherwise new edges might get inserted in the wrong place in the
// dictionary, and bad stuff will happen).
//
// We fix the problem by just splicing the offending vertex into the
// other edge.
func checkForLeftSplice(tess *tesselator, regUp *activeRegion) bool {
	regLo := regionBelow(regUp)
	eUp := regUp.eUp
	eLo := regLo.eUp

	assert(!vertEq(dst(eUp), dst(eLo)))

	if vertLeq(dst(eUp), dst(eLo)) {
		if tesedgeSign(dst(eUp), dst(eLo), eUp.Org) < 0 {
			return false
		}

		// eLo.Dst is above eUp, so splice eLo.Dst into eUp
		regionAbove(regUp).dirty = true
		regUp.dirty = true
		e := tessMeshSplitEdge(tess.mesh, eUp)
		tessMeshSplice(tess.mesh, eLo.Sym, e)
		e.Lface.inside = regUp.inside
	} else {
		if tesedgeSign(dst(eLo), dst(eUp), eLo.Org) > 0 {
			return false
		}
		// eUp.Dst is below eLo, so splice eUp.Dst into eLo
		regUp.dirty = true
		regLo.dirty = true
		e := tessMeshSplitEdge(tess.mesh, eLo)
		tessMeshSplice(tess.mesh, eUp.Lnext, eLo.Sym)
		rFace(e).inside = regUp.inside
	}
	return true
}

// Check the upper and lower edges of the given region to see if
// they intersect.  If so, create the intersection and add it
// to the data structures.
//
// Returns TRUE if adding the new intersection resulted in a recursive
// call to AddRightEdges(); in this case all "dirty" regions have been
// checked for intersections, and possibly regUp has been deleted.
func checkForIntersect(tess *tesselator, regUp *activeRegion) bool {
	regLo := regionBelow(regUp)
	eUp := regUp.eUp
	eLo := regLo.eUp
	orgUp := eUp.Org
	orgLo := eLo.Org
	dstUp := dst(eUp)
	dstLo := dst(eLo)

	assert(!vertEq(dstLo, dstUp))
	assert(tesedgeSign(dstUp, tess.event, orgUp) <= 0)
	assert(tesedgeSign(dstLo, tess.event, orgLo) >= 0)
	assert(orgUp != tess.event && orgLo != tess.event)
	assert(!regUp.fixUpperEdge && !regLo.fixUpperEdge)

	if orgUp == orgLo {
		// right endpoints are the same
		return false
	}

	tMinUp := minf(orgUp.t, dstUp.t)
	tMaxLo := maxf(orgLo.t, dstLo.t)
	if tMinUp > tMaxLo {
		// t ranges do not overlap
		return false
	}

	if vertLeq(orgUp, orgLo) {
		if tesedgeSign(dstLo, orgUp, orgLo) > 0 {
			return false
		}
	} else {
		if tesedgeSign(dstUp, orgLo, orgUp) < 0 {
			return false
		}
	}

	var isect vertex
	tesedgeIntersect(dstUp, orgUp, dstLo, orgLo, &isect)
	// The following properties are guaranteed:
	assert(minf(orgUp.t, dstUp.t) <= isect.t)
	assert(isect.t <= maxf(orgLo.t, dstLo.t))
	assert(minf(dstLo.s, dstUp.s) <= isect.s)
	assert(isect.s <= maxf(orgLo.s, orgUp.s))

	if vertLeq(&isect, tess.event) {
		// The intersection point lies slightly to the left of the sweep line,
		// so move it until it''s slightly to the right of the sweep line.
		// (If we had perfect numerical precision, this would never happen
		// in the first place).  The easiest and safest thing to do is
		// replace the intersection by tess.event.
		isect.s = tess.event.s
		isect.t = tess.event.t
	}
	// Similarly, if the computed intersection lies to the right of the
	// rightmost origin (which should rarely happen), it can cause
	// unbelievable inefficiency on sufficiently degenerate inputs.
	// (If you have the test program, try running test54.d with the
	// "X zoom" option turned on).
	var orgMin *vertex
	if vertLeq(orgUp, orgLo) {
		orgMin = orgUp
	} else {
		orgMin = orgLo
	}
	if vertLeq(orgMin, &isect) {
		isect.s = orgMin.s
		isect.t = orgMin.t
	}

	if vertEq(&isect, orgUp) || vertEq(&isect, orgLo) {
		// Easy case -- intersection at one of the right endpoints
		checkForRightSplice(tess, regUp)
		return false
	}

	if (!vertEq(dstUp, tess.event) && tesedgeSign(dstUp, tess.event, &isect) >= 0) || (!vertEq(dstLo, tess.event) && tesedgeSign(dstLo, tess.event, &isect) <= 0) {
		// Very unusual -- the new upper or lower edge would pass on the
		// wrong side of the sweep event, or through it.  This can happen
		// due to very small numerical errors in the intersection calculation.
		if dstLo == tess.event {
			// Splice dstLo into eUp, and process the new region(s)
			tessMeshSplitEdge(tess.mesh, eUp.Sym)
			tessMeshSplice(tess.mesh, eLo.Sym, eUp)
			regUp = topLeftRegion(tess, regUp)
			eUp = regionBelow(regUp).eUp
			finishLeftRegions(tess, regionBelow(regUp), regLo)
			addRightEdges(tess, regUp, oPrev(eUp), eUp, eUp, true)
			return true
		}
		if dstUp == tess.event {
			// Splice dstUp into eLo, and process the new region(s)
			tessMeshSplitEdge(tess.mesh, eLo.Sym)
			tessMeshSplice(tess.mesh, eUp.Lnext, oPrev(eLo))
			regLo = regUp
			regUp = topRightRegion(regUp)
			e := rPrev(regionBelow(regUp).eUp)
			regLo.eUp = oPrev(eLo)
			eLo = finishLeftRegions(tess, regLo, nil)
			addRightEdges(tess, regUp, eLo.Onext, rPrev(eUp), e, true)
			return true
		}
		// Special case: called from ConnectRightVertex.  If either
		// edge passes on the wrong side of tess.event, split it
		// (and wait for ConnectRightVertex to splice it appropriately).
		if tesedgeSign(dstUp, tess.event, &isect) >= 0 {
			regionAbove(regUp).dirty = true
			regUp.dirty = true
			tessMeshSplitEdge(tess.mesh, eUp.Sym)
			eUp.Org.s = tess.event.s
			eUp.Org.t = tess.event.t
		}
		if tesedgeSign(dstLo, tess.event, &isect) <= 0 {
			regUp.dirty = true
			regLo.dirty = true
			tessMeshSplitEdge(tess.mesh, eLo.Sym)
			eLo.Org.s = tess.event.s
			eLo.Org.t = tess.event.t
		}
		// leave the rest for ConnectRightVertex
		return false
	}

	// General case -- split both edges, splice into new vertex.
	// When we do the splice operation, the order of the arguments is
	// arbitrary as far as correctness goes.  However, when the operation
	// creates a new face, the work done is proportional to the size of
	// the new face.  We expect the faces in the processed part of
	// the mesh (ie. eUp.Lface) to be smaller than the faces in the
	// unprocessed original contours (which will be eLo.Oprev.Lface).
	tessMeshSplitEdge(tess.mesh, eUp.Sym)
	tessMeshSplitEdge(tess.mesh, eLo.Sym)
	tessMeshSplice(tess.mesh, oPrev(eLo), eUp)
	eUp.Org.s = isect.s
	eUp.Org.t = isect.t
	eUp.Org.pqHandle = pqInsert((*pq)(tess.pq), eUp.Org)
	getIntersectData(tess, eUp.Org, orgUp, dstUp, orgLo, dstLo)
	regionAbove(regUp).dirty = true
	regUp.dirty = true
	regLo.dirty = true
	return false
}

// walkDirtyRegions:
// When the upper or lower edge of any region changes, the region is
// marked "dirty".  This routine walks through all the dirty regions
// and makes sure that the dictionary invariants are satisfied
// (see the comments at the beginning of this file).  Of course
// new dirty regions can be created as we make changes to restore
// the invariants.
func walkDirtyRegions(tess *tesselator, regUp *activeRegion) {
	regLo := regionBelow(regUp)

	for {
		// Find the lowest dirty region (we walk from the bottom up).
		for regLo.dirty {
			regUp = regLo
			regLo = regionBelow(regLo)
		}
		if !regUp.dirty {
			regLo = regUp
			regUp = regionAbove(regUp)
			if regUp == nil || !regUp.dirty {
				// We've walked all the dirty regions
				return
			}
		}
		regUp.dirty = false
		eUp := regUp.eUp
		eLo := regLo.eUp

		if dst(eUp) != dst(eLo) {
			// Check that the edge ordering is obeyed at the Dst vertices.
			if checkForLeftSplice(tess, regUp) {

				// If the upper or lower edge was marked fixUpperEdge, then
				// we no longer need it (since these edges are needed only for
				// vertices which otherwise have no right-going edges).
				if regLo.fixUpperEdge {
					deleteRegion(tess, regLo)
					tessMeshDelete(tess.mesh, eLo)
					regLo = regionBelow(regUp)
					eLo = regLo.eUp
				} else if regUp.fixUpperEdge {
					deleteRegion(tess, regUp)
					tessMeshDelete(tess.mesh, eUp)
					regUp = regionAbove(regLo)
					eUp = regUp.eUp
				}
			}
		}
		if eUp.Org != eLo.Org {
			if dst(eUp) != dst(eLo) && !regUp.fixUpperEdge && !regLo.fixUpperEdge && (dst(eUp) == tess.event || dst(eLo) == tess.event) {
				// When all else fails in CheckForIntersect(), it uses tess.event
				// as the intersection location.  To make this possible, it requires
				// that tess.event lie between the upper and lower edges, and also
				// that neither of these is marked fixUpperEdge (since in the worst
				// case it might splice one of these edges into tess.event, and
				// violate the invariant that fixable edges are the only right-going
				// edge from their associated vertex).
				if checkForIntersect(tess, regUp) {
					// WalkDirtyRegions() was called recursively; we're done
					return
				}
			} else {
				// Even though we can't use CheckForIntersect(), the Org vertices
				// may violate the dictionary edge ordering.  Check and correct this.
				checkForRightSplice(tess, regUp)
			}
		}
		if eUp.Org == eLo.Org && dst(eUp) == dst(eLo) {
			// A degenerate loop consisting of only two edges -- delete it.
			addWinding(eLo, eUp)
			deleteRegion(tess, regUp)
			tessMeshDelete(tess.mesh, eUp)
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
func connectRightVertex(tess *tesselator, regUp *activeRegion, eBottomLeft *halfEdge) {
	eTopLeft := eBottomLeft.Onext
	regLo := regionBelow(regUp)
	eUp := regUp.eUp
	eLo := regLo.eUp
	degenerate := false

	if dst(eUp) != dst(eLo) {
		checkForIntersect(tess, regUp)
	}

	// Possible new degeneracies: upper or lower edge of regUp may pass
	// through vEvent, or may coincide with new intersection vertex
	if vertEq(eUp.Org, tess.event) {
		tessMeshSplice(tess.mesh, oPrev(eTopLeft), eUp)
		regUp = topLeftRegion(tess, regUp)
		eTopLeft = regionBelow(regUp).eUp
		finishLeftRegions(tess, regionBelow(regUp), regLo)
		degenerate = true
	}
	if vertEq(eLo.Org, tess.event) {
		tessMeshSplice(tess.mesh, eBottomLeft, oPrev(eLo))
		eBottomLeft = finishLeftRegions(tess, regLo, nil)
		degenerate = true
	}
	if degenerate {
		addRightEdges(tess, regUp, eBottomLeft.Onext, eTopLeft, eTopLeft, true)
		return
	}

	// Non-degenerate situation -- need to add a temporary, fixable edge.
	// Connect to the closer of eLo.Org, eUp.Org.
	var eNew *halfEdge
	if vertLeq(eLo.Org, eUp.Org) {
		eNew = oPrev(eLo)
	} else {
		eNew = eUp
	}
	eNew = tessMeshConnect(tess.mesh, lPrev(eBottomLeft), eNew)

	// Prevent cleanup, otherwise eNew might disappear before we've even
	// had a chance to mark it as a temporary edge.
	addRightEdges(tess, regUp, eNew, eNew.Onext, eNew.Onext, false)
	(*activeRegion)(eNew.Sym.activeRegion).fixUpperEdge = true
	walkDirtyRegions(tess, regUp)
}

// connectLeftDegenerate:
// The event vertex lies exacty on an already-processed edge or vertex.
// Adding the new vertex involves splicing it into the already-processed
// part of the mesh.
func connectLeftDegenerate(tess *tesselator, regUp *activeRegion, vEvent *vertex) {
	// Because vertices at exactly the same location are merged together
	// before we process the sweep event, some degenerate cases can't occur.
	// However if someone eventually makes the modifications required to
	// merge features which are close together, the cases below marked
	// TOLERANCE_NONZERO will be useful.  They were debugged before the
	// code to merge identical vertices in the main loop was added.
	const TOLERANCE_NONZERO = false

	e := regUp.eUp
	if vertEq(e.Org, vEvent) {
		// e.Org is an unprocessed vertex - just combine them, and wait
		// for e.Org to be pulled from the queue
		assert(TOLERANCE_NONZERO)
		spliceMergeVertices(tess, e, vEvent.anEdge)
		return
	}

	if vertEq(dst(e), vEvent) {
		// General case -- splice vEvent into edge e which passes through it
		tessMeshSplitEdge(tess.mesh, e.Sym)
		if regUp.fixUpperEdge {
			// This edge was fixable -- delete unused portion of original edge
			tessMeshDelete(tess.mesh, e.Onext)
			regUp.fixUpperEdge = false
		}
		tessMeshSplice(tess.mesh, vEvent.anEdge, e)
		// recurse
		sweepEvent(tess, vEvent)
		return
	}

	// vEvent coincides with e.Dst, which has already been processed.
	// Splice in the additional right-going edges.
	assert(TOLERANCE_NONZERO)
	regUp = topRightRegion(regUp)
	reg := regionBelow(regUp)
	eTopRight := reg.eUp.Sym
	eTopLeft := eTopRight.Onext
	eLast := eTopRight.Onext
	if reg.fixUpperEdge {
		// Here e.Dst has only a single fixable edge going right.
		// We can delete it since now we have some real right-going edges.
		assert(eTopLeft != eTopRight) // there are some left edges too
		deleteRegion(tess, reg)
		tessMeshDelete(tess.mesh, eTopRight)
		eTopRight = oPrev(eTopLeft)
	}
	tessMeshSplice(tess.mesh, vEvent.anEdge, eTopRight)
	if !edgeGoesLeft(eTopLeft) {
		// e.Dst had no left-going edges -- indicate this to AddRightEdges()
		eTopLeft = nil
	}
	addRightEdges(tess, regUp, eTopRight.Onext, eLast, eTopLeft, true)
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
func connectLeftVertex(tess *tesselator, vEvent *vertex) {
	var tmp activeRegion

	// assert( vEvent.anEdge.Onext.Onext == vEvent.anEdge );

	// Get a pointer to the active region containing vEvent
	tmp.eUp = vEvent.anEdge.Sym
	regUp := dictKey(dictSearch((*dict)(tess.dict), &tmp))
	regLo := regionBelow(regUp)
	if regLo == nil {
		// This may happen if the input polygon is coplanar.
		return
	}
	eUp := regUp.eUp
	eLo := regLo.eUp

	// Try merging with U or L first
	if tesedgeSign(dst(eUp), vEvent, eUp.Org) == 0 {
		connectLeftDegenerate(tess, regUp, vEvent)
		return
	}

	// Connect vEvent to rightmost processed vertex of either chain.
	// e.Dst is the vertex that we will connect to vEvent.
	var reg *activeRegion
	if vertLeq(dst(eLo), dst(eUp)) {
		reg = regUp
	} else {
		reg = regLo
	}

	if regUp.inside || reg.fixUpperEdge {
		var eNew *halfEdge
		if reg == regUp {
			eNew = tessMeshConnect(tess.mesh, vEvent.anEdge.Sym, eUp.Lnext)
		} else {
			tempHalfEdge := tessMeshConnect(tess.mesh, dNext(eLo), vEvent.anEdge)
			eNew = tempHalfEdge.Sym
		}
		if reg.fixUpperEdge {
			fixUpperEdge(tess, reg, eNew)
		} else {
			computeWinding(tess, addRegionBelow(tess, regUp, eNew))
		}
		sweepEvent(tess, vEvent)
	} else {
		// The new vertex is in a region which does not belong to the polygon.
		// We don''t need to connect this vertex to the rest of the mesh.
		addRightEdges(tess, regUp, vEvent.anEdge, vEvent.anEdge, nil, true)
	}
}

// sweepEvent does everything necessary when the sweep line crosses a vertex.
// Updates the mesh and the edge dictionary.
func sweepEvent(tess *tesselator, vEvent *vertex) {
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
	regUp := topLeftRegion(tess, (*activeRegion)(e.activeRegion))
	reg := regionBelow(regUp)
	eTopLeft := reg.eUp
	eBottomLeft := finishLeftRegions(tess, reg, nil)

	// Next we process all the right-going edges from vEvent.  This
	// involves adding the edges to the dictionary, and creating the
	// associated "active regions" which record information about the
	// regions between adjacent dictionary edges.
	if eBottomLeft.Onext == eTopLeft {
		// No right-going edges -- add a temporary "fixable" edge
		connectRightVertex(tess, regUp, eBottomLeft)
	} else {
		addRightEdges(tess, regUp, eBottomLeft.Onext, eTopLeft, eTopLeft, true)
	}
}

// addSentinel makes the sentinel coordinates big enough that they will never be
// merged with real input features.
//
// We add two sentinel edges above and below all other edges,
// to avoid special cases at the top and bottom.
func addSentinel(tess *tesselator, smin, smax float, t float) {
	reg := &activeRegion{}

	e := tessMeshMakeEdge(tess.mesh)

	e.Org.s = smax
	e.Org.t = t
	dst(e).s = smin
	dst(e).t = t
	tess.event = dst(e)

	reg.eUp = e
	reg.sentinel = true
	reg.nodeUp = dictInsert((*dict)(tess.dict), reg)
}

// initEdgeDict:
// We maintain an ordering of edge intersections with the sweep line.
// This order is maintained in a dynamic dictionary.
func initEdgeDict(tess *tesselator) {
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

func doneEdgeDict(tess *tesselator) {
	fixedEdges := 0
	for {
		reg := dictKey(dictMin((*dict)(tess.dict)))
		if reg == nil {
			break
		}
		// At the end of all processing, the dictionary should contain
		// only the two sentinel edges, plus at most one "fixable" edge
		// created by ConnectRightVertex().
		if !reg.sentinel {
			assert(reg.fixUpperEdge)
			fixedEdges++
			assert(fixedEdges == 1)
		}
		assert(reg.windingNumber == 0)
		deleteRegion(tess, reg)
		// tessMeshDelete( reg.eUp );
	}
}

// removeDegenerateEdges removes zero-length edges, and contours with fewer than 3 vertices.
func removeDegenerateEdges(tess *tesselator) {
	eHead := &tess.mesh.eHead
	var eNext *halfEdge
	for e := eHead.next; e != eHead; e = eNext {
		eNext = e.next
		eLnext := e.Lnext

		if vertEq(e.Org, dst(e)) && e.Lnext.Lnext != e {
			// Zero-length edge, contour has at least 3 edges
			spliceMergeVertices(tess, eLnext, e) /* deletes e.Org */
			tessMeshDelete(tess.mesh, e)
			e = eLnext
			eLnext = e.Lnext
		}
		if eLnext.Lnext == e {
			// Degenerate contour (one or two edges)
			if eLnext != e {
				if eLnext == eNext || eLnext == eNext.Sym {
					eNext = eNext.next
				}
				tessMeshDelete(tess.mesh, eLnext)
			}
			if e == eNext || e == eNext.Sym {
				eNext = eNext.next
			}
			tessMeshDelete(tess.mesh, e)
		}
	}
}

// initPriorityQ inserts all vertices into the priority queue which determines the
// order in which vertices cross the sweep line.
func initPriorityQ(tess *tesselator) {
	vertexCount := C.int(0)

	vHead := &tess.mesh.vHead
	for v := vHead.next; v != vHead; v = v.next {
		vertexCount++
	}
	// Make sure there is enough space for sentinels.
	vertexCount += 8

	tess.pq = pqNewPriorityQ(vertexCount)

	vHead = &tess.mesh.vHead
	for v := vHead.next; v != vHead; v = v.next {
		v.pqHandle = pqInsert((*pq)(tess.pq), v)
	}
}

// removeDegenerateFaces deletes any degenerate faces with only two edges.  walkDirtyRegions()
// will catch almost all of these, but it won't catch degenerate faces
// produced by splice operations on already-processed edges.
// The two places this can happen are in FinishLeftRegions(), when
// we splice in a "temporary" edge produced by ConnectRightVertex(),
// and in checkForLeftSplice(), where we splice already-processed
// edges to ensure that our dictionary invariants are not violated
// by numerical errors.
//
// In both these cases it is *very* dangerous to delete the offending
// edge at the time, since one of the routines further up the stack
// will sometimes be keeping a pointer to that edge.
func removeDegenerateFaces(tess *tesselator, mesh *mesh) {
	var fNext *face
	for f := mesh.fHead.next; f != &mesh.fHead; f = fNext {
		fNext = f.next
		e := f.anEdge
		assert(e.Lnext != e)

		if e.Lnext.Lnext == e {
			// A face with only two edges
			addWinding(e.Onext, e)
			tessMeshDelete(tess.mesh, e)
		}
	}
}

// tessComputeInterior computes the planar arrangement specified
// by the given contours, and further subdivides this arrangement
// into regions.  Each region is marked "inside" if it belongs
// to the polygon, according to the rule given by tess.windingRule.
// Each interior region is guaranteed be monotone.
func tessComputeInterior(tess *tesselator) {
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
		v := pqExtractMin((*pq)(tess.pq))
		if v == nil {
			break
		}
		for {
			vNext := (*vertex)(pqMinimum((*pq)(tess.pq)))
			if vNext == nil || !vertEq(vNext, v) {
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
			vNext = pqExtractMin((*pq)(tess.pq))
			spliceMergeVertices(tess, v.anEdge, vNext.anEdge)
		}
		sweepEvent(tess, v)
	}

	// Set tess.event for debugging purposes
	tess.event = dictKey(dictMin((*dict)(tess.dict))).eUp.Org
	doneEdgeDict(tess)

	removeDegenerateFaces(tess, tess.mesh)
	tessMeshCheckMesh(tess.mesh)
}
