#!/bin/bash

cd ../tools

python convert_prima_to_coco.py \
    --prima_datapath ../destination \
    --anno_savepath ../destination/annotations.json 

python train_net.py \
    --dataset_name          emj \
    --json_annotation_train ../destination/result.json \
    --image_path_train      ../destination \
    --json_annotation_val   ../destination/control.json \
    --image_path_val        ../destination \
    --config-file           ../configs/config.yaml \
    OUTPUT_DIR  ../outputs/fourthtest/ \
    SOLVER.IMS_PER_BATCH 2 