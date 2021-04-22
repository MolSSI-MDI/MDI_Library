#ifndef MPI_STUB
#define MPI_STUB

typedef int MPI_Comm;
typedef int MPI_Datatype;
typedef int MPI_Status;

#define MPI_MAX_PROCESSOR_NAME 0
#define MPI_STATUS_IGNORE 0
#define MPI_COMM_WORLD 0
#define MPI_INT 1
#define MPI_DOUBLE 4
#define MPI_CHAR 5

int MPI_Init( int *argc, char ***argv) { return 0;};
int MPI_Finalize(void) { return 0; };

int MPI_Comm_rank( MPI_Comm comm, int *rank ) {
  *rank = 0;
  return 0;
};
int MPI_Comm_size( MPI_Comm comm, int *size ) {
  *size = 1;
  return 0;
};
int MPI_Get_processor_name( char *name, int *resultlen ) {return 0;};

int MPI_Barrier(MPI_Comm comm) { return 0; };
int MPI_Bcast(void *buffer, int count, MPI_Datatype datatype, int root, MPI_Comm comm) { return 0; }
int MPI_Gather(const void *sendbuf, int sendcount, MPI_Datatype sendtype,
               void *recvbuf, int recvcount, MPI_Datatype recvtype,
               int root, MPI_Comm comm) { return 0; };
int MPI_Send(const void *buf, int count, MPI_Datatype datatype, int dest, int tag,
             MPI_Comm comm) { return 0; };
int MPI_Recv(void *buf, int count, MPI_Datatype datatype, int source, int tag,
             MPI_Comm comm, MPI_Status *status) { return 0; };
int MPI_Comm_split(MPI_Comm comm, int color, int key, MPI_Comm *newcomm) { return 0; };
int MPI_Abort(MPI_Comm comm, int errorcode) { return 0; };

#endif
