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

struct CListDouble *listDouble (void) {
    struct CListDouble *init;
    init = malloc (sizeof (struct CListDouble));

    int length = 63;
    double array[length];
    array[0] = 1.0;
    array[1] = 2.0;
    for (int i = 2; i < length; i++) {
        array[i] = array[i-1] + array[i-2];
    }
    struct CListDouble *elem = init;
    elem->value = array[0];
    for (int i = 1; i < 63; i++) {
        elem->next = malloc (sizeof (struct CListDouble));
        elem = elem->next;
        elem->value = array[i];
    }
    return init;
}

struct CListInt *listInt (void) {
    struct CListInt *init;
    init = malloc (sizeof (struct CListInt));

    struct CListInt *elem = init;
    elem->value = 0;
    for (int i = 1; i < 42; i++) {
        elem->next = malloc (sizeof (struct CListInt));
        elem = elem->next;
        elem->value = i;
    }
    return init;
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

