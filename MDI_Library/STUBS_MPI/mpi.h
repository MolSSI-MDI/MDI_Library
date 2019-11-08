#ifndef MDI_MPI_STUBS
#define MDI_MPI_STUBS

#include <stdlib.h>
#include <stdint.h>
#include <string.h>

typedef int MPI_Comm;
typedef int MPI_Datatype;
typedef int MPI_Status;
typedef int MPI_Fint;

#define MPI_STATUS_IGNORE 0
#define MPI_COMM_WORLD 0
#define MPI_INT 1
#define MPI_DOUBLE 4
#define MPI_CHAR 5

static int MPI_Init( int *argc, char ***argv) { return 0;};
static int MPI_Finalize(void) { return 0; };
static int MPI_Get_processor_name( char *name, int *resultlen ) {return 0;};

static int MPI_Comm_rank( MPI_Comm comm, int *rank ) { return 0; };
static int MPI_Comm_size( MPI_Comm comm, int *size ) { return 0; };

static int MPI_Barrier(MPI_Comm comm) { return 0; };
static int MPI_Gather(const void *sendbuf, int sendcount, MPI_Datatype sendtype,
               void *recvbuf, int recvcount, MPI_Datatype recvtype,
               int root, MPI_Comm comm) { return 0; };
static int MPI_Allgather(const void *sendbuf, int sendcount, MPI_Datatype sendtype,
               void *recvbuf, int recvcount, MPI_Datatype recvtype,
               MPI_Comm comm) { return 0; };
static int MPI_Send(const void *buf, int count, MPI_Datatype datatype, int dest, int tag,
             MPI_Comm comm) { return 0; };
static int MPI_Recv(void *buf, int count, MPI_Datatype datatype, int source, int tag,
             MPI_Comm comm, MPI_Status *status) { return 0; };
static int MPI_Comm_split(MPI_Comm comm, int color, int key, MPI_Comm *newcomm) { return 0; };
static MPI_Comm MPI_Comm_f2c( MPI_Fint comm ) { return comm; };
static MPI_Fint MPI_Comm_c2f( MPI_Comm comm ) { return comm; };

#endif
