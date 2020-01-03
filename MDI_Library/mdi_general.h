/*! \file
 *
 * \brief Generic MDI function calls
 */

#ifndef MDI_GENERAL
#define MDI_GENERAL

#include "mdi.h"
#include "mdi_global.h"

/*! \brief Function pointer to the generic execute_command function */
extern int (*execute_command)(const char*, MDI_Comm);

int general_init(const char* options, void* world_comm);
int general_accept_communicator();
int general_send(const void* buf, int count, MDI_Datatype datatype, MDI_Comm comm);
int general_recv(void* buf, int count, MDI_Datatype datatype, MDI_Comm comm);
int general_send_command(const char* buf, MDI_Comm comm);
int general_recv_command(char* buf, MDI_Comm comm);
int general_builtin_command(const char* buf, MDI_Comm comm);

int register_node(vector* node_vec, const char* node_name);
int register_command(vector* node_vec, const char* node_name, const char* command_name);
int register_callback(vector* node_vec, const char* node_name, const char* callback_name);

int send_command_list(MDI_Comm comm);
int send_callback_list(MDI_Comm comm);
int send_node_list(MDI_Comm comm);
int send_ncommands(MDI_Comm comm);
int send_ncallbacks(MDI_Comm comm);
int send_nnodes(MDI_Comm comm);
int get_node_info(MDI_Comm comm);
vector* get_node_vector(MDI_Comm comm);

#endif
