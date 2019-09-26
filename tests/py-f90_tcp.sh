python driver_py.py -mdi "-role DRIVER -name driver -method TCP -port 8021" &
./engine_f90 -mdi "-role ENGINE -name MM -method TCP -port 8021 -hostname localhost" &
wait
