#!/bin/bash
cp ../bin/lj .
cp ../Ctrl .

#mkdir -p proc_000064/run001
#cd proc_000064/run001/
#mpirun -n 512 -npernode 8 -bysocket -bind-to-core numactl -l ../../lj \
#        max_phys 8,8,8 \
#        subs 8,8,8 \
#        pernode 2,2 \
#        npart 512000000 \
#        ../../Ctrl 
#
#cd ../..
mkdir -p proc_000064/run001
cd proc_000064/run001/
mpirun -n 64 -npernode 8 -bysocket -bind-to-core numactl -l ../../lj \
        max_phys 4,4,4 \
        subs 4,4,4 \
        pernode 2,2,2 \
        npart 64000000 \
        ../../Ctrl 

cd ../..
mkdir -p proc_000008/run001
cd proc_000008/run001/
mpirun -n 8 -npernode 8 -bysocket -bind-to-core numactl -l ../../lj \
        max_phys 2,2,2 \
        subs 2,2,2 \
        pernode 2,2,2 \
        npart 8000000 \
        ../../Ctrl 

cd ../..
mkdir -p proc_000001/run001
cd proc_000001/run001/
mpirun -n 1 -npernode 1 -bysocket -bind-to-core numactl -l ../../lj \
        max_phys 1,1,1 \
        npart 1000000 \
        ../../Ctrl 

cd ../..
