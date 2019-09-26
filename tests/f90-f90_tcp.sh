./$(find driver_f90*) -mdi "-role DRIVER -name driver -method TCP -port 8021" &
./$(find engine_f90*) -mdi "-role ENGINE -name MM -method TCP -port 8021 -hostname localhost" &
wait
