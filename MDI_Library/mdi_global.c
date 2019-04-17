/*! \file
 *
 * \brief Global structures used by MDI
 */

#include <stdio.h>
//#include <string.h>
//#include <stdlib.h>
//#include <unistd.h>
#include "mdi_global.h"

static vector communicators;

int vector_init(vector* v, size_t stride) {
  //initialize the vector with the given stride
  v->data = malloc(0);
  if (!v->data) {
    perror("Could not initialize vector");
    exit(-1);
  }

  v->size = 0;
  v->capacity = 0;
  v->stride = stride;

  return 0;
}

int vector_push_back(vector* v, void* element) {
  //grow the vector
  if (v->size >= v->capacity) {
    int new_capacity;
    if ( v->capacity > 0 ) {
      new_capacity = 2 * v->capacity;
    }
    else {
      new_capacity = 1;
    }
    void* new_data = malloc( v->stride * new_capacity );
    memcpy(new_data, v->data, v->size * v->stride);
    free(v->data);
    v->data = new_data;
    v->capacity = new_capacity;
  }

  //add the new data to the vector
  memcpy( v->data + (v->size * v->stride), (unsigned char*)element, v->stride );
  v->size++;

  return 0;
}

void* vector_get(vector* v, int index) {
  if (index < 0 || index >= v->size) {
    mdi_error("Vector accessed out-of-bounds");
  }
  return ( void* )( v->data + (index * v->stride) );
}
