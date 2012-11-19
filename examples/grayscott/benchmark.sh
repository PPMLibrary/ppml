#!/bin/bash
cp ../bin/grayscott .
cp ../Ctrl .

mkdir -p proc_000256/run001
cd proc_000256/run001/
mpirun -n 256 -npernode 16 -bycore -bind-to-core ../../grayscott \
        max_phys 160,160 \
        subs 16,16 \
        pernode 4,4 \
        npart 256000000 \
        ../../Ctrl 

cd ../..
mkdir -p proc_000064/run001
cd proc_000064/run001/
mpirun -n 64 -npernode 16 -bycore -bind-to-core ../../grayscott \
        max_phys 80,80 \
        subs 8,8 \
        pernode 4,4 \
        npart 64000000 \
        ../../Ctrl 

cd ../..
mkdir -p proc_000016/run001
cd proc_000016/run001/
mpirun -n 16 -npernode 16 -bycore -bind-to-core ../../grayscott \
        max_phys 40,40 \
        subs 4,4 \
        pernode 4,4 \
        npart 16000000 \
        ../../Ctrl 

cd ../..
mkdir -p proc_000004/run001
cd proc_000004/run001/
mpirun -n 4 -npernode 16 -bycore -bind-to-core ../../grayscott \
        max_phys 20,20 \
        subs 2,2 \
        pernode 2,2 \
        npart 4000000 \
        ../../Ctrl 

cd ../..
mkdir -p proc_000001/run001
cd proc_000004/run001/
mpirun -n 1 -npernode 16 -bycore -bind-to-core ../../grayscott \
        max_phys 10,10 \
        npart 1000000 \
        ../../Ctrl 

cd ../..
