#include <stdlib.h>
#include "c_test.h"

struct CArrayDouble *arrayDouble (void) {
    struct CArrayDouble *a;
    a = malloc (sizeof (struct CArrayDouble));

    a->length = 63;
    a->array = malloc (a->length * sizeof (double));
    a->array[0] = 1.0;
    a->array[1] = 2.0;

    for (int i = 2; i < a->length; i++) {
        a->array[i] = a->array[i-1] + a->array[i-2];
    }
    return a;
}

struct CArrayInt *arrayInt (void) {
    struct CArrayInt *a;
    a = malloc (sizeof (struct CArrayInt));

    a->length = 42;
    a->array = malloc (a->length * sizeof (int));

    for (int i = 0; i < a->length; i++) {
        a->array[i] = i;
    }
    return a;
}

struct CArrayFloat *arrayFloat (void) {
    struct CArrayFloat *a;
    a = malloc (sizeof (struct CArrayFloat));

    a->length = 21;
    a->array = malloc (a->length * sizeof (float));

    for (int i = 0; i < a->length; i++) {
        a->array[i] = (float) i/2.0;
    }
    return a;
}

/*
 * Hasky's linked lists do not have to be allocated in a single space.
 * In fact Hasky-Types itself allocates memory for each element individually.
 * However, if the C equivalents are build with individual allocation per element,
 * on Travis-CI cabal fails with:
 *
 *            $ cabal: failed to create OS thread: Cannot allocate memory
 *
 * This issue is fixed when allocating the list array like, which is sufficient for
 * the CI tests.
 */

struct CListDouble *listDouble (void) {
    int length = 63;
    struct CListDouble *head;
    head = malloc (length * sizeof (struct CListDouble));

    // Create array with fibs
    double array[length];
    array[0] = 1.0;
    array[1] = 2.0;
    for (int i = 2; i < length; i++) {
        array[i] = array[i-1] + array[i-2];
    }

    // Build linked list with data
    struct CListDouble *elem = head;
    elem->value = array[0];
    for (int i = 1; i < length; i++) {
        elem->next = (struct CListDouble *) head+i;
        elem = elem->next;
        elem->value = array[i];
    }
    elem->next = NULL;
    return head;
}

struct CListInt *listInt (void) {
    int length = 42;
    struct CListInt *head;
    head = malloc (length * sizeof (struct CListInt));

    struct CListInt *elem = head;
    elem->value = 0;
    for (int i = 1; i < length; i++) {
        elem->next = (struct CListInt *) head+i;
        elem = elem->next;
        elem->value = i;
    }
    elem->next = NULL;
    return head;
}

struct CListFloat *listFloat (void) {
    struct CListFloat *init;
    init = malloc (sizeof (struct CListFloat));

    struct CListFloat *elem = init;
    elem->value = 0.0f;
    for (int i = 1; i < 21; i++) {
        elem->next = malloc (sizeof (struct CListFloat));
        elem = elem->next;
        elem->value = (float) i/2.0;
    }
    return init;
}

