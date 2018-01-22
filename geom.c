/*
** SGI FREE SOFTWARE LICENSE B (Version 2.0, Sept. 18, 2008) 
** Copyright (C) [dates of first publication] Silicon Graphics, Inc.
** All Rights Reserved.
**
** Permission is hereby granted, free of charge, to any person obtaining a copy
** of this software and associated documentation files (the "Software"), to deal
** in the Software without restriction, including without limitation the rights
** to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
** of the Software, and to permit persons to whom the Software is furnished to do so,
** subject to the following conditions:
** 
** The above copyright notice including the dates of first publication and either this
** permission notice or a reference to http://oss.sgi.com/projects/FreeB/ shall be
** included in all copies or substantial portions of the Software. 
**
** THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
** INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
** PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL SILICON GRAPHICS, INC.
** BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
** TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE
** OR OTHER DEALINGS IN THE SOFTWARE.
** 
** Except as contained in this notice, the name of Silicon Graphics, Inc. shall not
** be used in advertising or otherwise to promote the sale, use or other dealings in
** this Software without prior written authorization from Silicon Graphics, Inc.
*/
/*
** Author: Eric Veach, July 1994.
*/

//#include "tesos.h"
#include <assert.h>
#include "mesh.h"
#include "geom.h"

int tesvertLeq( TESSvertex *u, TESSvertex *v )
{
	/* Returns TRUE if u is lexicographically <= v. */

	return VertLeq( u, v );
}

TESSreal tesedgeEval( TESSvertex *u, TESSvertex *v, TESSvertex *w )
{
	/* Given three vertices u,v,w such that VertLeq(u,v) && VertLeq(v,w),
	* evaluates the t-coord of the edge uw at the s-coord of the vertex v.
	* Returns v->t - (uw)(v->s), ie. the signed distance from uw to v.
	* If uw is vertical (and thus passes thru v), the result is zero.
	*
	* The calculation is extremely accurate and stable, even when v
	* is very close to u or w.  In particular if we set v->t = 0 and
	* let r be the negated result (this evaluates (uw)(v->s)), then
	* r is guaranteed to satisfy MIN(u->t,w->t) <= r <= MAX(u->t,w->t).
	*/
	TESSreal gapL, gapR;

	assert( VertLeq( u, v ) && VertLeq( v, w ));

	gapL = v->s - u->s;
	gapR = w->s - v->s;

	if( gapL + gapR > 0 ) {
		if( gapL < gapR ) {
			return (v->t - u->t) + (u->t - w->t) * (gapL / (gapL + gapR));
		} else {
			return (v->t - w->t) + (w->t - u->t) * (gapR / (gapL + gapR));
		}
	}
	/* vertical line */
	return 0;
}

TESSreal tesedgeSign( TESSvertex *u, TESSvertex *v, TESSvertex *w )
{
	/* Returns a number whose sign matches EdgeEval(u,v,w) but which
	* is cheaper to evaluate.  Returns > 0, == 0 , or < 0
	* as v is above, on, or below the edge uw.
	*/
	TESSreal gapL, gapR;

	assert( VertLeq( u, v ) && VertLeq( v, w ));

	gapL = v->s - u->s;
	gapR = w->s - v->s;

	if( gapL + gapR > 0 ) {
		return (v->t - w->t) * gapL + (v->t - u->t) * gapR;
	}
	/* vertical line */
	return 0;
}


/***********************************************************************
* Define versions of EdgeSign, EdgeEval with s and t transposed.
*/

TESSreal testransEval( TESSvertex *u, TESSvertex *v, TESSvertex *w )
{
	/* Given three vertices u,v,w such that TransLeq(u,v) && TransLeq(v,w),
	* evaluates the t-coord of the edge uw at the s-coord of the vertex v.
	* Returns v->s - (uw)(v->t), ie. the signed distance from uw to v.
	* If uw is vertical (and thus passes thru v), the result is zero.
	*
	* The calculation is extremely accurate and stable, even when v
	* is very close to u or w.  In particular if we set v->s = 0 and
	* let r be the negated result (this evaluates (uw)(v->t)), then
	* r is guaranteed to satisfy MIN(u->s,w->s) <= r <= MAX(u->s,w->s).
	*/
	TESSreal gapL, gapR;

	assert( TransLeq( u, v ) && TransLeq( v, w ));

	gapL = v->t - u->t;
	gapR = w->t - v->t;

	if( gapL + gapR > 0 ) {
		if( gapL < gapR ) {
			return (v->s - u->s) + (u->s - w->s) * (gapL / (gapL + gapR));
		} else {
			return (v->s - w->s) + (w->s - u->s) * (gapR / (gapL + gapR));
		}
	}
	/* vertical line */
	return 0;
}

TESSreal testransSign( TESSvertex *u, TESSvertex *v, TESSvertex *w )
{
	/* Returns a number whose sign matches TransEval(u,v,w) but which
	* is cheaper to evaluate.  Returns > 0, == 0 , or < 0
	* as v is above, on, or below the edge uw.
	*/
	TESSreal gapL, gapR;

	assert( TransLeq( u, v ) && TransLeq( v, w ));

	gapL = v->t - u->t;
	gapR = w->t - v->t;

	if( gapL + gapR > 0 ) {
		return (v->s - w->s) * gapL + (v->s - u->s) * gapR;
	}
	/* vertical line */
	return 0;
}


int tesvertCCW( TESSvertex *u, TESSvertex *v, TESSvertex *w )
{
	/* For almost-degenerate situations, the results are not reliable.
	* Unless the floating-point arithmetic can be performed without
	* rounding errors, *any* implementation will give incorrect results
	* on some degenerate inputs, so the client must have some way to
	* handle this situation.
	*/
	return (u->s*(v->t - w->t) + v->s*(w->t - u->t) + w->s*(u->t - v->t)) >= 0;
}

int VertEq(TESSvertex* u, TESSvertex* v) {
  return (u)->s == (v)->s && (u)->t == (v)->t;
}

int VertLeq(TESSvertex* u, TESSvertex* v) {
  return ((u)->s < (v)->s) || ((u)->s == (v)->s && (u)->t <= (v)->t);
}

TESSreal VertL1dist(TESSvertex* u, TESSvertex* v) {
  return ABS(u->s - v->s) + ABS(u->t - v->t);
}

int EdgeGoesLeft(TESShalfEdge* e) {
  return VertLeq((e)->Dst, (e)->Org);
}

int EdgeGoesRight(TESShalfEdge* e) {
  return VertLeq((e)->Org, (e)->Dst);
}

int TransLeq(TESSvertex* u, TESSvertex* v) {
  return ((u)->t < (v)->t) || ((u)->t == (v)->t && (u)->s <= (v)->s);
}
