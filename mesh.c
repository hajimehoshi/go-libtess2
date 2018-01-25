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

/* MakeVertex( newVertex, eOrig, vNext ) attaches a new vertex and makes it the
* origin of all edges in the vertex loop to which eOrig belongs. "vNext" gives
* a place to insert the new vertex in the global vertex list.  We insert
* the new vertex *before* vNext so that algorithms which walk the vertex
* list will not see the newly created vertices.
*/
static void MakeVertex( TESSvertex *newVertex, 
					   TESShalfEdge *eOrig, TESSvertex *vNext )
{
	TESShalfEdge *e;
	TESSvertex *vPrev;
	TESSvertex *vNew = newVertex;

	assert(vNew != NULL);

	/* insert in circular doubly-linked list before vNext */
	vPrev = vNext->prev;
	vNew->prev = vPrev;
	vPrev->next = vNew;
	vNew->next = vNext;
	vNext->prev = vNew;

	vNew->anEdge = eOrig;
	/* leave coords, s, t undefined */

	/* fix other edges on this vertex loop */
	e = eOrig;
	do {
		e->Org = vNew;
		e = e->Onext;
	} while( e != eOrig );
}

/* MakeFace( newFace, eOrig, fNext ) attaches a new face and makes it the left
* face of all edges in the face loop to which eOrig belongs.  "fNext" gives
* a place to insert the new face in the global face list.  We insert
* the new face *before* fNext so that algorithms which walk the face
* list will not see the newly created faces.
*/
void MakeFace( TESSface *newFace, TESShalfEdge *eOrig, TESSface *fNext )
{
	TESShalfEdge *e;
	TESSface *fPrev;
	TESSface *fNew = newFace;

	assert(fNew != NULL); 

	/* insert in circular doubly-linked list before fNext */
	fPrev = fNext->prev;
	fNew->prev = fPrev;
	fPrev->next = fNew;
	fNew->next = fNext;
	fNext->prev = fNew;

	fNew->anEdge = eOrig;
	fNew->trail = NULL;
	fNew->marked = FALSE;

	/* The new face is marked "inside" if the old one was.  This is a
	* convenience for the common case where a face has been split in two.
	*/
	fNew->inside = fNext->inside;

	/* fix other edges on this face loop */
	e = eOrig;
	do {
		e->Lface = fNew;
		e = e->Lnext;
	} while( e != eOrig );
}

/* KillEdge( eDel ) destroys an edge (the half-edges eDel and eDel->Sym),
* and removes from the global edge list.
*/
void KillEdge( TESSmesh *mesh, TESShalfEdge *eDel )
{
	TESShalfEdge *ePrev, *eNext;

	/* Half-edges are allocated in pairs, see EdgePair above */
	if( eDel->Sym < eDel ) { eDel = eDel->Sym; }

	/* delete from circular doubly-linked list */
	eNext = eDel->next;
	ePrev = eDel->Sym->next;
	eNext->Sym->next = ePrev;
	ePrev->Sym->next = eNext;

	bucketFree( mesh->edgeBucket, eDel );
}


/* KillVertex( vDel ) destroys a vertex and removes it from the global
* vertex list.  It updates the vertex loop to point to a given new vertex.
*/
void KillVertex( TESSmesh *mesh, TESSvertex *vDel, TESSvertex *newOrg )
{
	TESShalfEdge *e, *eStart = vDel->anEdge;
	TESSvertex *vPrev, *vNext;

	/* change the origin of all affected edges */
	e = eStart;
	do {
		e->Org = newOrg;
		e = e->Onext;
	} while( e != eStart );

	/* delete from circular doubly-linked list */
	vPrev = vDel->prev;
	vNext = vDel->next;
	vNext->prev = vPrev;
	vPrev->next = vNext;

	bucketFree( mesh->vertexBucket, vDel );
}

/* KillFace( fDel ) destroys a face and removes it from the global face
* list.  It updates the face loop to point to a given new face.
*/
void KillFace( TESSmesh *mesh, TESSface *fDel, TESSface *newLface )
{
	TESShalfEdge *e, *eStart = fDel->anEdge;
	TESSface *fPrev, *fNext;

	/* change the left face of all affected edges */
	e = eStart;
	do {
		e->Lface = newLface;
		e = e->Lnext;
	} while( e != eStart );

	/* delete from circular doubly-linked list */
	fPrev = fDel->prev;
	fNext = fDel->next;
	fNext->prev = fPrev;
	fPrev->next = fNext;

	bucketFree( mesh->faceBucket, fDel );
}


/****************** Basic Edge Operations **********************/

/* tessMeshMakeEdge creates one edge, two vertices, and a loop (face).
* The loop consists of the two new half-edges.
*/
TESShalfEdge *tessMeshMakeEdge( TESSmesh *mesh )
{
	TESSvertex *newVertex1 = (TESSvertex*)bucketAlloc(mesh->vertexBucket);
	TESSvertex *newVertex2 = (TESSvertex*)bucketAlloc(mesh->vertexBucket);
	TESSface *newFace = (TESSface*)bucketAlloc(mesh->faceBucket);
	TESShalfEdge *e;

	/* if any one is null then all get freed */
	if (newVertex1 == NULL || newVertex2 == NULL || newFace == NULL) {
		if (newVertex1 != NULL) bucketFree( mesh->vertexBucket, newVertex1 );
		if (newVertex2 != NULL) bucketFree( mesh->vertexBucket, newVertex2 );
		if (newFace != NULL) bucketFree( mesh->faceBucket, newFace );     
		return NULL;
	} 

	e = MakeEdge( mesh, &mesh->eHead );
	if (e == NULL) return NULL;

	MakeVertex( newVertex1, e, &mesh->vHead );
	MakeVertex( newVertex2, e->Sym, &mesh->vHead );
	MakeFace( newFace, e, &mesh->fHead );
	return e;
}


/* tessMeshSplice( eOrg, eDst ) is the basic operation for changing the
* mesh connectivity and topology.  It changes the mesh so that
*	eOrg->Onext <- OLD( eDst->Onext )
*	eDst->Onext <- OLD( eOrg->Onext )
* where OLD(...) means the value before the meshSplice operation.
*
* This can have two effects on the vertex structure:
*  - if eOrg->Org != eDst->Org, the two vertices are merged together
*  - if eOrg->Org == eDst->Org, the origin is split into two vertices
* In both cases, eDst->Org is changed and eOrg->Org is untouched.
*
* Similarly (and independently) for the face structure,
*  - if eOrg->Lface == eDst->Lface, one loop is split into two
*  - if eOrg->Lface != eDst->Lface, two distinct loops are joined into one
* In both cases, eDst->Lface is changed and eOrg->Lface is unaffected.
*
* Some special cases:
* If eDst == eOrg, the operation has no effect.
* If eDst == eOrg->Lnext, the new face will have a single edge.
* If eDst == eOrg->Lprev, the old face will have a single edge.
* If eDst == eOrg->Onext, the new vertex will have a single edge.
* If eDst == eOrg->Oprev, the old vertex will have a single edge.
*/
int tessMeshSplice( TESSmesh* mesh, TESShalfEdge *eOrg, TESShalfEdge *eDst )
{
	int joiningLoops = FALSE;
	int joiningVertices = FALSE;

	if( eOrg == eDst ) return 1;

	if( eDst->Org != eOrg->Org ) {
		/* We are merging two disjoint vertices -- destroy eDst->Org */
		joiningVertices = TRUE;
		KillVertex( mesh, eDst->Org, eOrg->Org );
	}
	if( eDst->Lface != eOrg->Lface ) {
		/* We are connecting two disjoint loops -- destroy eDst->Lface */
		joiningLoops = TRUE;
		KillFace( mesh, eDst->Lface, eOrg->Lface );
	}

	/* Change the edge structure */
	Splice( eDst, eOrg );

	if( ! joiningVertices ) {
		TESSvertex *newVertex = (TESSvertex*)bucketAlloc( mesh->vertexBucket );
		if (newVertex == NULL) return 0;

		/* We split one vertex into two -- the new vertex is eDst->Org.
		* Make sure the old vertex points to a valid half-edge.
		*/
		MakeVertex( newVertex, eDst, eOrg->Org );
		eOrg->Org->anEdge = eOrg;
	}
	if( ! joiningLoops ) {
		TESSface *newFace = (TESSface*)bucketAlloc( mesh->faceBucket );  
		if (newFace == NULL) return 0;

		/* We split one loop into two -- the new loop is eDst->Lface.
		* Make sure the old face points to a valid half-edge.
		*/
		MakeFace( newFace, eDst, eOrg->Lface );
		eOrg->Lface->anEdge = eOrg;
	}

	return 1;
}


/* tessMeshDelete( eDel ) removes the edge eDel.  There are several cases:
* if (eDel->Lface != eDel->Rface), we join two loops into one; the loop
* eDel->Lface is deleted.  Otherwise, we are splitting one loop into two;
* the newly created loop will contain eDel->Dst.  If the deletion of eDel
* would create isolated vertices, those are deleted as well.
*
* This function could be implemented as two calls to tessMeshSplice
* plus a few calls to memFree, but this would allocate and delete
* unnecessary vertices and faces.
*/
int tessMeshDelete( TESSmesh *mesh, TESShalfEdge *eDel )
{
	TESShalfEdge *eDelSym = eDel->Sym;
	int joiningLoops = FALSE;

	/* First step: disconnect the origin vertex eDel->Org.  We make all
	* changes to get a consistent mesh in this "intermediate" state.
	*/
	if( eDel->Lface != eDel->Rface ) {
		/* We are joining two loops into one -- remove the left face */
		joiningLoops = TRUE;
		KillFace( mesh, eDel->Lface, eDel->Rface );
	}

	if( eDel->Onext == eDel ) {
		KillVertex( mesh, eDel->Org, NULL );
	} else {
		/* Make sure that eDel->Org and eDel->Rface point to valid half-edges */
		eDel->Rface->anEdge = eDel->Oprev;
		eDel->Org->anEdge = eDel->Onext;

		Splice( eDel, eDel->Oprev );
		if( ! joiningLoops ) {
			TESSface *newFace= (TESSface*)bucketAlloc( mesh->faceBucket );
			if (newFace == NULL) return 0; 

			/* We are splitting one loop into two -- create a new loop for eDel. */
			MakeFace( newFace, eDel, eDel->Lface );
		}
	}

	/* Claim: the mesh is now in a consistent state, except that eDel->Org
	* may have been deleted.  Now we disconnect eDel->Dst.
	*/
	if( eDelSym->Onext == eDelSym ) {
		KillVertex( mesh, eDelSym->Org, NULL );
		KillFace( mesh, eDelSym->Lface, NULL );
	} else {
		/* Make sure that eDel->Dst and eDel->Lface point to valid half-edges */
		eDel->Lface->anEdge = eDelSym->Oprev;
		eDelSym->Org->anEdge = eDelSym->Onext;
		Splice( eDelSym, eDelSym->Oprev );
	}

	/* Any isolated vertices or faces have already been freed. */
	KillEdge( mesh, eDel );

	return 1;
}


/******************** Other Edge Operations **********************/

/* All these routines can be implemented with the basic edge
* operations above.  They are provided for convenience and efficiency.
*/


/* tessMeshAddEdgeVertex( eOrg ) creates a new edge eNew such that
* eNew == eOrg->Lnext, and eNew->Dst is a newly created vertex.
* eOrg and eNew will have the same left face.
*/
TESShalfEdge *tessMeshAddEdgeVertex( TESSmesh *mesh, TESShalfEdge *eOrg )
{
	TESShalfEdge *eNewSym;
	TESShalfEdge *eNew = MakeEdge( mesh, eOrg );
	if (eNew == NULL) return NULL;

	eNewSym = eNew->Sym;

	/* Connect the new edge appropriately */
	Splice( eNew, eOrg->Lnext );

	/* Set the vertex and face information */
	eNew->Org = eOrg->Dst;
	{
		TESSvertex *newVertex= (TESSvertex*)bucketAlloc( mesh->vertexBucket );
		if (newVertex == NULL) return NULL;

		MakeVertex( newVertex, eNewSym, eNew->Org );
	}
	eNew->Lface = eNewSym->Lface = eOrg->Lface;

	return eNew;
}
