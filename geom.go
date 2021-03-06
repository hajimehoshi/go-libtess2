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

func vertEq(u, v *vertex) bool {
	return u.s == v.s && u.t == v.t
}

func vertLeq(u, v *vertex) bool {
	return (u.s < v.s) || (u.s == v.s && u.t <= v.t)
}

func vertL1dist(u, v *vertex) float {
	return abs(u.s-v.s) + abs(u.t-v.t)
}

func transLeq(u, v *vertex) bool {
	return (u.t < v.t) || (u.t == v.t && u.s <= v.s)
}

func edgeGoesLeft(e *halfEdge) bool {
	return vertLeq(e.dst(), e.Org)
}

func edgeGoesRight(e *halfEdge) bool {
	return vertLeq(e.Org, e.dst())
}

// edgeEval:
// Given three vertices u,v,w such that vertLeq(u,v) && vertLeq(v,w),
// evaluates the t-coord of the edge uw at the s-coord of the vertex v.
// Returns v.t - (uw)(v.s), ie. the signed distance from uw to v.
// If uw is vertical (and thus passes thru v), the result is zero.
//
// The calculation is extremely accurate and stable, even when v
// is very close to u or w.  In particular if we set v.t = 0 and
// let r be the negated result (this evaluates (uw)(v.s)), then
// r is guaranteed to satisfy MIN(u.t,w.t) <= r <= MAX(u.t,w.t).
func edgeEval(u, v, w *vertex) float {
	assert(vertLeq(u, v) && vertLeq(v, w))

	gapL := v.s - u.s
	gapR := w.s - v.s

	if gapL+gapR > 0 {
		if gapL < gapR {
			return (v.t - u.t) + (u.t-w.t)*(gapL/(gapL+gapR))
		} else {
			return (v.t - w.t) + (w.t-u.t)*(gapR/(gapL+gapR))
		}
	}
	// vertical line
	return 0
}

// edgeSign returns a number whose sign matches EdgeEval(u,v,w) but which
// is cheaper to evaluate.  Returns > 0, == 0 , or < 0
// as v is above, on, or below the edge uw.
func edgeSign(u, v, w *vertex) float {
	assert(vertLeq(u, v) && vertLeq(v, w))

	gapL := v.s - u.s
	gapR := w.s - v.s

	if gapL+gapR > 0 {
		return (v.t-w.t)*gapL + (v.t-u.t)*gapR
	}
	// vertical line
	return 0
}

// transEval:
// Given three vertices u,v,w such that transLeq(u,v) && transLeq(v,w),
// evaluates the t-coord of the edge uw at the s-coord of the vertex v.
// Returns v.s - (uw)(v.t), ie. the signed distance from uw to v.
// If uw is vertical (and thus passes thru v), the result is zero.
//
// The calculation is extremely accurate and stable, even when v
// is very close to u or w.  In particular if we set v.s = 0 and
// let r be the negated result (this evaluates (uw)(v.t)), then
// r is guaranteed to satisfy MIN(u.s,w.s) <= r <= MAX(u.s,w.s).
func transEval(u, v, w *vertex) float {
	assert(transLeq(u, v) && transLeq(v, w))

	gapL := v.t - u.t
	gapR := w.t - v.t

	if gapL+gapR > 0 {
		if gapL < gapR {
			return (v.s - u.s) + (u.s-w.s)*(gapL/(gapL+gapR))
		} else {
			return (v.s - w.s) + (w.s-u.s)*(gapR/(gapL+gapR))
		}
	}
	// vertical line
	return 0
}

// transSign returns a number whose sign matches TransEval(u,v,w) but which
// is cheaper to evaluate.  Returns > 0, == 0 , or < 0
// as v is above, on, or below the edge uw.
func transSign(u, v, w *vertex) float {
	assert(transLeq(u, v) && transLeq(v, w))

	gapL := v.t - u.t
	gapR := w.t - v.t

	if gapL+gapR > 0 {
		return (v.s-w.s)*gapL + (v.s-u.s)*gapR
	}
	// vertical line
	return 0
}

// vertCCW:
// For almost-degenerate situations, the results are not reliable.
// Unless the floating-point arithmetic can be performed without
// rounding errors, *any* implementation will give incorrect results
// on some degenerate inputs, so the client must have some way to
// handle this situation.
func vertCCW(u, v, w *vertex) bool {
	return (u.s*(v.t-w.t) + v.s*(w.t-u.t) + w.s*(u.t-v.t)) >= 0
}

// interpolate:
// Given parameters a,x,b,y returns the value (b*x+a*y)/(a+b),
// or (x+y)/2 if a==b==0.  It requires that a,b >= 0, and enforces
// this in the rare case that one argument is slightly negative.
// The implementation is extremely stable numerically.
// In particular it guarantees that the result r satisfies
// MIN(x,y) <= r <= MAX(x,y), and the results are very accurate
// even when a and b differ greatly in magnitude.
func interpolate(a, x, b, y float) float {
	if a < 0 {
		a = 0
	}
	if b < 0 {
		b = 0
	}
	if a <= b {
		if b == 0 {
			return (x + y) / 2
		}
		return x + (y-x)*(a/(a+b))
	}
	return y + (x-y)*(b/(a+b))
}

// edgeIntersect:
// Given edges (o1,d1) and (o2,d2), compute their point of intersection.
// The computed point is guaranteed to lie in the intersection of the
// bounding rectangles defined by each edge.
func edgeIntersect(o1 *vertex, d1 *vertex, o2 *vertex, d2 *vertex, v *vertex) {
	// This is certainly not the most efficient way to find the intersection
	// of two line segments, but it is very numerically stable.
	//
	// Strategy: find the two middle vertices in the vertLeq ordering,
	// and interpolate the intersection s-value from these.  Then repeat
	// using the transLeq ordering to find the intersection t-value.

	if !vertLeq(o1, d1) {
		o1, d1 = d1, o1
	}
	if !vertLeq(o2, d2) {
		o2, d2 = d2, o2
	}
	if !vertLeq(o1, o2) {
		o1, o2 = o2, o1
		d1, d2 = d2, d1
	}

	if !vertLeq(o2, d1) {
		// Technically, no intersection -- do our best
		v.s = (o2.s + d1.s) / 2
	} else if vertLeq(d1, d2) {
		// Interpolate between o2 and d1
		z1 := edgeEval(o1, o2, d1)
		z2 := edgeEval(o2, d1, d2)
		if z1+z2 < 0 {
			z1 = -z1
			z2 = -z2
		}
		v.s = interpolate(z1, o2.s, z2, d1.s)
	} else {
		// Interpolate between o2 and d2
		z1 := edgeSign(o1, o2, d1)
		z2 := -edgeSign(o1, d2, d1)
		if z1+z2 < 0 {
			z1 = -z1
			z2 = -z2
		}
		v.s = interpolate(z1, o2.s, z2, d2.s)
	}

	// Now repeat the process for t

	if !transLeq(o1, d1) {
		o1, d1 = d1, o1
	}
	if !transLeq(o2, d2) {
		o2, d2 = d2, o2
	}
	if !transLeq(o1, o2) {
		o1, o2 = o2, o1
		d1, d2 = d2, d1
	}

	if !transLeq(o2, d1) {
		// Technically, no intersection -- do our best
		v.t = (o2.t + d1.t) / 2
	} else if transLeq(d1, d2) {
		// Interpolate between o2 and d1
		z1 := transEval(o1, o2, d1)
		z2 := transEval(o2, d1, d2)
		if z1+z2 < 0 {
			z1 = -z1
			z2 = -z2
		}
		v.t = interpolate(z1, o2.t, z2, d1.t)
	} else {
		// Interpolate between o2 and d2
		z1 := transSign(o1, o2, d1)
		z2 := -transSign(o1, d2, d1)
		if z1+z2 < 0 {
			z1 = -z1
			z2 = -z2
		}
		v.t = interpolate(z1, o2.t, z2, d2.t)
	}
}
