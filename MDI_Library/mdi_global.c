/*! \file
 *
 * \brief Global structures used by MDI
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "mdi_global.h"

/*! \brief Whether MDI is running in i-PI compatibility mode */
int ipi_compatibility = 0;

/*! \brief Vector containing all MDI communicators */
vector communicators;

/*! \brief Vector containing all nodes supported by this code */
vector nodes;

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

/*! \brief Determine the index of a node within a vector of nodes
 *
 * \param [in]       v
 *                   Pointer to the vector
 * \param [in]       node_name
 *                   Name of the node
 */
int get_node_index(vector* v, const char* node_name) {
  int inode;
  int node_index = -1;
  for ( inode = 0; inode < v->size; inode++ ) {
    node* this_node = vector_get(v, inode);
    if ( strcmp( node_name, this_node->name ) == 0 ) {
      node_index = inode;
    }
  }
  return node_index;
}

/*! \brief Determine the index of a command within a node
 *
 * \param [in]       node
 *                   Pointer to the node
 * \param [in]       command_name
 *                   Name of the command
 */
int get_command_index(node* n, const char* command_name) {
  int icommand;
  int command_index = -1;
  for ( icommand = 0; icommand < n->commands->size; icommand++ ) {
    if ( strcmp( command_name, vector_get( n->commands, icommand ) ) == 0 ) {
      command_index = icommand;
    }
  }
  return command_index;
}

/*! \brief Determine the index of a callback within a node
 *
 * \param [in]       node
 *                   Pointer to the node
 * \param [in]       callback_name
 *                   Name of the callback
 */
int get_callback_index(node* n, const char* callback_name) {
  int icallback;
  int callback_index = -1;
  for ( icallback = 0; icallback < n->callbacks->size; icallback++ ) {
    if ( strcmp( callback_name, vector_get( n->callbacks, icallback ) ) == 0 ) {
      callback_index = icallback;
    }
  }
  return callback_index;
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
