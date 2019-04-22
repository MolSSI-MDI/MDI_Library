/*! \file
 *
 * \brief Global structures used by MDI
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "mdi_global.h"


/*! \brief Vector containing all MDI communicators */
vector communicators;

/*! \brief Initialize memory allocation for a vector structure
 *
 * \param [in]       v
 *                   Pointer to the vector structure for which the memory will be allocated
 * \param [in]       stride
 *                   Stride of the vector
 */
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

/*! \brief Append a new element to the end of the vector
 *
 * \param [in]       v
 *                   Pointer to the vector to which the element will be appended
 * \param [in]       element
 *                   Pointer to the element that will be appended to the vector
 */
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

/*! \brief Return a pointer to an element of a vector
 *
 * \param [in]       v
 *                   Pointer to the vector
 * \param [in]       index
 *                   Index of the element within the vector
 */
void* vector_get(vector* v, int index) {
  if (index < 0 || index >= v->size) {
    mdi_error("Vector accessed out-of-bounds");
  }
  return ( void* )( v->data + (index * v->stride) );
}


/*! \brief Print error message and exit
 *
 * \param [in]       message
 *                   Message printed before exiting.
 */
void mdi_error(const char* message) {
  perror(message);
  exit(1);
}
