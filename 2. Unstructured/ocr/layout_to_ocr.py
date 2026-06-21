#!/usr/bin/env python
# coding: utf-8

import requests
import numpy as np
import layoutparser as lp
import glob
from PIL import Image as im
import PIL
import pandas as pd
import random
import cv2
import argparse
import pdf2image
from pdf2image import convert_from_path
import matplotlib.pyplot as plt
import layoutparser.ocr as ocr
import re
from numpy import asarray
import pytesseract
pytesseract.pytesseract.tesseract_cmd = (r'/usr/bin/tesseract')


parser = argparse.ArgumentParser(
	description="transforms the layout information into ocr"
)
parser.add_argument("--configpath", type=str, help="Where to find the yaml file")
parser.add_argument("--modelpath", type=str, help="Where to find the pth file")
parser.add_argument("--pagepath", type=str, help="Where to find the csv with the page information")
parser.add_argument("--pdfpath", type=str, help="Where to find the files in pdf")

config = "../outputs/fourthtest/config.yaml"
model = "../outputs/fourthtest/model_final.pth"
pages = '../notebooks/pag1.csv'
pdfs = '../pdf/'


model = lp.Detectron2LayoutModel(
    config_path = config,
    model_path = model,
    label_map   = {0: "Author", 1: "Date", 2: "Page", 3:"Text", 4:"Title"}, 
    extra_config = ["MODEL.ROI_HEADS.SCORE_THRESH_TEST", 0.75] # <-- Only output high accuracy preds
)
ocr_agent = ocr.TesseractAgent(languages='eng')
todo = pd.read_csv(pages)
path1  = pdfs
todo['path']= path1 + todo['name']
for i in range(0, len(todo)):
        b_blocks={'x1':[], 'y1':[],'x2':[],'y2':[],
              'type':[], 'text':[], 'score':[], 'page':[], 'height':[], 'wide':[]}
        x=todo['pageind'][i]+1
        y=todo['pagef'][i]-5
        for h in range(x, y):
            imagen= convert_from_path(todo['path'][i], first_page=h, last_page=h) #, output_folder=path3
            z= "".join([str(todo['volume'][i]),'_',str(h)])
            try:
                image = asarray(imagen[0])
                layout = model.detect(image)
                layout.page_data = z
                for j in range(len(layout)):
                    segment_image = (layout[j].block
                                              .pad(left=5, right=5, top=5, bottom=5)
                                              .crop_image(image))
                    text = ocr_agent.detect(segment_image)
                    layout[j].set(text=text, inplace=True)
                    b_blocks['x1'].append(layout[j].block.x_1)
                    b_blocks['y1'].append(layout[j].block.y_1)
                    b_blocks['x2'].append(layout[j].block.x_2)
                    b_blocks['y2'].append(layout[j].block.y_2)
                    b_blocks['type'].append(layout[j].type)
                    b_blocks['text'].append(layout[j].text)
                    b_blocks['score'].append(layout[j].score)
                    b_blocks['page'].append(layout.page_data)
                    b_blocks['height'].append(image.shape[0])
                    b_blocks['wide'].append(image.shape[1])
            except IndexError:
                    pass
            imagen =[]
            image =[]
            layout =[]
            z =[]
        bloques=pd.DataFrame.from_dict(b_blocks)
        b_blocks = []
        bloques=bloques.sort_values(by=['page','y1', 'x1'])
        filename= str(todo['volume'][i])+'.csv'
        bloques.to_csv(filename, index = True)
        bloques = []

