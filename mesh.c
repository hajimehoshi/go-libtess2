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
#include <stddef.h>
#include <assert.h>
#include "mesh.h"
#include "bucketalloc.h"

#define TRUE 1
#define FALSE 0

/************************ Utility Routines ************************/

/* MakeEdge creates a new pair of half-edges which form their own loop.
* No vertex or face structures are allocated, but these must be assigned
* before the current edge operation is completed.
*/
TESShalfEdge *MakeEdge( TESSmesh* mesh, TESShalfEdge *eNext )
{
	TESShalfEdge *e;
	TESShalfEdge *eSym;
	TESShalfEdge *ePrev;
	EdgePair *pair = (EdgePair *)bucketAlloc( mesh->edgeBucket );
	if (pair == NULL) return NULL;

	e = &pair->e;
	eSym = &pair->eSym;

	/* Make sure eNext points to the first edge of the edge pair */
	if( eNext->Sym < eNext ) { eNext = eNext->Sym; }

	/* Insert in circular doubly-linked list before eNext.
	* Note that the prev pointer is stored in Sym->next.
	*/
	ePrev = eNext->Sym->next;
	eSym->next = ePrev;
	ePrev->Sym->next = e;
	e->next = eNext;
	eNext->Sym->next = eSym;

	e->Sym = eSym;
	e->Onext = e;
	e->Lnext = eSym;
	e->Org = NULL;
	e->Lface = NULL;
	e->winding = 0;
	e->activeRegion = NULL;

	eSym->Sym = e;
	eSym->Onext = eSym;
	eSym->Lnext = e;
	eSym->Org = NULL;
	eSym->Lface = NULL;
	eSym->winding = 0;
	eSym->activeRegion = NULL;

	return e;
}

/* Splice( a, b ) is best described by the Guibas/Stolfi paper or the
* CS348a notes (see mesh.h).  Basically it modifies the mesh so that
* a->Onext and b->Onext are exchanged.  This can have various effects
* depending on whether a and b belong to different face or vertex rings.
* For more explanation see tessMeshSplice() below.
*/
void Splice( TESShalfEdge *a, TESShalfEdge *b )
{
	TESShalfEdge *aOnext = a->Onext;
	TESShalfEdge *bOnext = b->Onext;

	aOnext->Sym->Lnext = b;
	bOnext->Sym->Lnext = a;
	a->Onext = bOnext;
	b->Onext = aOnext;
}
