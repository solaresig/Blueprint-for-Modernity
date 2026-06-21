# Distant reading of the Engineering and Mining Journal
## Application of detectron2, layoutparser and label studio
### Project: Blueprint for Modernity. 

This notebook summarizes the process of creating an Engineering a Mining Journal database using computer vision and labeling using the free tools of detectron2, layoutparser, and label studio. Primarily, this exercise follows the steps developed by [lolipopshock](https://github.com/Layout-Parser/layout-parser/blob/main/examples/Customizing%20Layout%20Models%20with%20Label%20Studio%20Annotation/Customizing%20Layout%20Models%20with%20Label%20Studio%20Annotation.ipynb) 

The process is composed of six steps:
1. Preparation of the training datasets <b>
2. Labeling of the datasets <b>
3. Training and evaluation <b>
4. Prediction using the model <b>
5. Construction of the database <b>
6. Evaluation of the database 

### 1. Preparation of the database
This step allowed the researchers,to provide an example of labeling using labelstudio for a research assistant, who labeled a representative sample of the dataset, composed of 99 volumes of the Engineering and Mining Journal, downloaded from [Hathitrust](https://catalog.hathitrust.org/Record/000052973) from 1872 to 1923. 
#### Construction of the training dataset 


```python
pip install detectron2 https://dl.fbaipublicfiles.com/detectron2/wheels/cu113/torch1.10/index.html
```

    Defaulting to user installation because normal site-packages is not writeable
    Collecting https://dl.fbaipublicfiles.com/detectron2/wheels/cu113/torch1.10/index.html
      Using cached https://dl.fbaipublicfiles.com/detectron2/wheels/cu113/torch1.10/index.html (468 bytes)
    [31m  ERROR: Cannot unpack file /tmp/pip-unpack-6g2f3tqa/index.html (downloaded from /tmp/pip-req-build-33t0u1p0, content-type: text/html); cannot detect archive format[0m[31m
    [0m[31mERROR: Cannot determine archive format of /tmp/pip-req-build-33t0u1p0[0m[31m
    [0mNote: you may need to restart the kernel to use updated packages.



```python
import requests
import numpy as np
import layoutparser as lp
import glob
from PIL import Image as im
import PIL
import pandas as pd
import random
import cv2
import pdf2image
from pdf2image import convert_from_path
import matplotlib.pyplot as plt
```

Loading the list of files and creating a random list of items that will be labeled


```python
train = pd.read_csv('pag2.csv')
todo = pd.read_csv('pag1.csv')
filelist= train['name']
path1  = '/home/.../ensayo/emj/'
path2 = '/home/.../ensayo/img/'
path3 = '/home/..../ensayo/entrenamiento/'
train['path']= path1 + train['name']
todo['path']= path1 + todo['name']
todo['pageind']=todo['pageind']+1
imagenes= []
prueba =[]
```


```python
for i in range(0, len(train)):
    imagen= convert_from_path(train['path'][i], first_page=train['page1'][i], last_page=train['page2'][i])
    k = int(round(len(imagen)*0.69, 0))
    muestra= random.sample(imagen, k)
    for j in range(0, len(muestra)):
        filename= "".join([path3, str(train['volume'][i]),"_", str(j) ,".jpg"])
        picture = muestra[j]
        picture = picture.save(filename)
```

After the first dataset was labeled, we created a second random set of items to be labeled.


```python
todo = pd.read_csv('pag1.csv')
filelist= todo['name']
path1  = '/home/.../ensayo/emj/'
path2 = '/home/.../ensayo/img/'
path3 = '/home/.../ensayo/entrenamiento2/'
todo['path']= path1 + todo['name']
todo['page1']=todo['pageind']+300
todo['page2']=todo['pageind']+400
imagenes= []
prueba =[]
```


```python
for i in range(0, len(todo)):
    try:
        imagen= convert_from_path(todo['path'][i], first_page=todo['page1'][i], last_page=todo['page2'][i])
        k = 3
        muestra= random.sample(imagen, k)
    except ValueError:
        imagen= convert_from_path(todo['path'][i], first_page=todo['pageind'][i])
        k = 3
        muestra= random.sample(imagen, k)
    for j in range(0, len(muestra)):
        filename= "".join([path3, str(todo['volume'][i]),"_", str(j) ,".jpg"])
        picture = muestra[j]
        picture = picture.save(filename)
    
```


```python
for i in range(56, len(todo)):
    try:
        imagen= convert_from_path(todo['path'][i], first_page=todo['page1'][i], last_page=todo['page2'][i])
        k = 3
        muestra= random.sample(imagen, k)
    except ValueError:
        imagen= convert_from_path(todo['path'][i], first_page=todo['pageind'][i])
        k = 3
        muestra= random.sample(imagen, k)
    for j in range(0, len(muestra)):
        filename= "".join([path3, str(todo['volume'][i]),"_", str(j) ,".jpg"])
        picture = muestra[j]
        picture = picture.save(filename)
    
```

### 2. Labeling
The two datasets were labeled by Jack Wang following the examples of a first test and using the process proposed by label studio.
As the labeling was made on multiple computers, the ids of the coco annotations are inconsistent, so the consolidation requires homogenizing the labeling ids. Furthermore, the PI also double-checked the labeling tasks, eliminating ambiguities in the process.


```python
pip install detectron2 https://dl.fbaipublicfiles.com/detectron2/wheels/cu113/torch1.10/index.html
pip install -U scikit-learn scipy matplotlib
```


```python
import requests
import numpy as np
import layoutparser as lp
import glob
from PIL import Image as img
import re
import pandas as pd
import json
from pycocotools.coco import COCO
import layoutparser as lp
import random
import cv2
from numpy import asarray
```


```python
def load_coco_annotations(annotations, coco=None):
    """
    Args:
        annotations (List):
            a list of coco annotaions for the current image
        coco (`optional`, defaults to `False`):
            COCO annotation object instance. If set, this function will
            convert the loaded annotation category ids to category names
            set in COCO.categories
    """
    layout = lp.Layout()

    for ele in annotations:

        x, y, w, h = ele['bbox']

        layout.append(
            lp.TextBlock(
                block = lp.Rectangle(x, y, w+x, h+y),
                type  = ele['category_id'] if coco is None else coco.cats[ele['category_id']]['name'],
                id = ele['id']
            )
        )

    return layout
```

Uploading the three files coming from the human labeling of the random samples


```python
listarchivos=glob.glob('/origin/*/result.json')
listarchivos
```

Evaluating the categories of the files


```python
f1 = open(listarchivos[0])
image_ann1 = json.load(f1)
f2 = open(listarchivos[1])
image_ann2 = json.load(f2)
f3 = open(listarchivos[2])
image_ann3 = json.load(f3)
image_ann1['categories']
```


```python
image_ann2['categories']
```


```python
image_ann3['categories']
```

Compiling the files into a single one, and correcting the connections


```python
rep= 'images/(\d)/'
image_list={'images':[], 
            'categories':[{'id': 0, 'name': 'Author'},
                          {'id': 1, 'name': 'Date'},
                          {'id': 2, 'name': 'Page'},
                          {'id': 3, 'name': 'Text'},
                          {'id': 4, 'name': 'Title'}], 
            'annotations':[],  
            'info': {'year': 2023, 'version': '1.0', 'description': '', 'contributor': 'Label Studio', 'url': '', 'date_created': '2023-03-07 17:43:40.748757'}
           }
```


```python
for j in range(0, len(listarchivos)):
    f = open(listarchivos[j])
    image_ann = json.load(f)
    for i in range(0, len(image_ann['images'])):
        x= image_ann['images'][i]['id']+len(image_list['images'])
        y=re.findall(r'/.*|\\.*', image_ann['images'][i]['file_name'])[0]
        y=y.replace('\\', '/')
        y=y.replace('/4/', '/')
        y=y.replace('///', '/')
        y="images/"+y
        image_ann['images'][i]['id']= x
        image_ann['images'][i]['file_name']= y
    for i in range(0, len(image_ann['annotations'])):
        x= image_ann['annotations'][i]['id']+len(image_list['annotations'])
        y= image_ann['annotations'][i]['image_id']+len(image_list['images'])
        image_ann['annotations'][i]['id']= x
        image_ann['annotations'][i]['image_id']= y
    image_list['images'].extend(image_ann['images'])
    image_list['annotations'].extend(image_ann['annotations'])
```


```python
with open('/destination/resultbeta.json', 'w') as outfile:
    json.dump(image_list, outfile)
```

After visually looking at the labeling, the PI eliminated the inconsistencies in the images. 


```python
COCO_ANNO_PATH = '/destination/resultbeta.json'
COCO_IMG_PATH  = '/destination'
COCO_IMG_TEST = '/corroboration'
coco = COCO(COCO_ANNO_PATH)
```


```python
for image_id in coco.imgs:
    image_info = coco.imgs[image_id]
    annotations = coco.loadAnns(coco.getAnnIds([image_id]))
    image = cv2.imread(f'{COCO_IMG_PATH}/{image_info["file_name"]}')
    layout = load_coco_annotations(annotations, coco)
    viz = lp.draw_box(image, layout, show_element_type=True)
    path = f'{COCO_IMG_TEST}/{image_info["file_name"]}'
    viz.save(path)
```

Example of a labeled image ![0c662ec3-00000043.jpg](attachment:0c662ec3-00000043.jpg)

After eliminating the ambiguous labeling, we proceed to redo the JSON file


```python
listimgs=glob.glob('/corroboration/images/*.jpg')
listimgs2 = pd.DataFrame(listimgs)
listimgs2='images//'+listimgs2[0].str.extract(r'((?<=images[/]).*.jpg)')
listimgs2=listimgs2[0].values.tolist()
len(listimgs2)
```




    514




```python
f3 = open('/home/.../ensayoigs/ensayo3/destination/resultbeta.json')
image_ann = json.load(f3)
image_list={'images':[], 
            'categories':[{'id': 0, 'name': 'Author'},
                          {'id': 1, 'name': 'Date'},
                          {'id': 2, 'name': 'Page'},
                          {'id': 3, 'name': 'Text'},
                          {'id': 4, 'name': 'Title'}], 
            'annotations':[],  
            'info': {'year': 2023, 'version': '1.0', 'description': '', 'contributor': 'Label Studio', 'url': '', 'date_created': '2023-03-07 17:43:40.748757'}
           }
ids = []
for i in range(0, len(image_ann['images'])):
    if image_ann['images'][i]['file_name'] in listimgs2:
        ids.append(image_ann['images'][i]['id'])
for i in range(0, len(image_ann['images'])):
    if image_ann['images'][i]['file_name'] in listimgs2:
        image_list['images'].append(image_ann['images'][i])
    else :
        pass        
for i in range(0, len(image_ann['annotations'])):
    if image_ann['annotations'][i]['image_id'] in ids:
        image_list['annotations'].append(image_ann['annotations'][i])
    else :
        pass
```

Comparing the two files


```python
len(image_ann['images'])-len(image_list['images'])
```




    36




```python
len(image_ann['annotations'])-len(image_list['annotations'])
```




    142



Selecting a random sample for training and evaluation sets. 


```python
k = round(len(image_list['images'])*.15)
lista1 = random.sample(image_list['images'], k)
lista = []
for i in range(0, len(lista1)):
    x= lista1[i]['id']
    lista.append(x)
```


```python
result={'images':[], 
            'categories':[{'id': 0, 'name': 'Author'},
                          {'id': 1, 'name': 'Date'},
                          {'id': 2, 'name': 'Page'},
                          {'id': 3, 'name': 'Text'},
                          {'id': 4, 'name': 'Title'}], 
            'annotations':[],  
            'info': {'year': 2023, 'version': '1.0', 'description': '', 'contributor': 'Label Studio', 'url': '', 'date_created': '2023-03-07 17:43:40.748757'}
           }
control={'images':[], 
            'categories':[{'id': 0, 'name': 'Author'},
                          {'id': 1, 'name': 'Date'},
                          {'id': 2, 'name': 'Page'},
                          {'id': 3, 'name': 'Text'},
                          {'id': 4, 'name': 'Title'}], 
            'annotations':[],  
            'info': {'year': 2023, 'version': '1.0', 'description': '', 'contributor': 'Label Studio', 'url': '', 'date_created': '2023-03-07 17:43:40.748757'}
           }
for i in range(0, len(image_list['images'])):
    if image_list['images'][i]['id'] in lista:
        control['images'].append(image_list['images'][i])
    else:
        result['images'].append(image_list['images'][i])   
for i in range(0, len(image_list['annotations'])):
    if image_list['annotations'][i]['image_id'] in lista:
        control['annotations'].append(image_list['annotations'][i])
    else :
        result['annotations'].append(image_list['annotations'][i])
```

Saving the control and the training group


```python
with open('/destination/result.json', 'w') as outfile:
    json.dump(result, outfile)
with open('/destination/control.json', 'w') as outfile:
    json.dump(control, outfile)
```

 Alternatively, the splitting can be made using the code proposed by lolipopshock <b>
    
 python cocosplit.py \
	--annotation-path     ../destination/result.json \
	--split-ratio         0.83 \
	--train               ../destination/train.json \
	--test                ../destination/test.json

### 3. Training and evaluation


We used the  GPUs provided by Google Cloud. These are the steps followed.

1. Choose a 100GB disk with the Debian image for ML
2. Be sure that Nvidia drivers are in. If not, look for the Nvidia drivers<br>
https://la.nvidia.com/content/DriverDownloads/confirmation.php?url=/tesla/460.106.00/NVIDIA-Linux-x86_64-460.106.00.run&lang=us&type=Tesla<br>
sudo chmod +x filename.run
3. Be sure to have the latest cuda 11.3 <br>
wget https://developer.download.nvidia.com/compute/cuda/repos/ubuntu1804/x86_64/cuda-keyring_1.0-1_all.deb <br>
sudo dpkg -i cuda-keyring_1.0-1_all.deb <br>
sudo apt-get update <br>
sudo apt-get -y install cuda <br>
4. Install torch. <br>
pip install torch==1.10.1 torchvision==0.11.2 torchaudio==0.10.1 <br>
conda install pytorch==1.10.1 torchvision==0.11.2 torchaudio==0.10.1 cudatoolkit=11.3 -c pytorch -c conda-forge <br>
5. Install detectron2 <br>
python3 -m pip install detectron2 -f \https://dl.fbaipublicfiles.com/detectron2/wheels/cu113/torch1.10/index.html <br>
6. Use the interaction with bucket <br>
gsutil -m cp -r gs://... /home/.../ <br>
7. There might be a mistake in setup utils; remove it. <br>
AttributeError: module 'distutils' has no attribute 'version' <br>
https://stackoverflow.com/questions/70520120/attributeerror-module-setuptools-distutils-has-no-attribute-version <br>
8. If there is this mistake <br>
ImportError: libtorch_cuda_cu.so: cannot open shared object file: No such file or directory <br>
Then check the installation of torch (step 4) <br>
9. check if Imagesize and Funcy are present, if not <br>
pip install imagesize <br>
pip install funcy <br>
10. run train_fourth.sh <br>
Note that the file train_net.py merges weights from "model_init.pth", the result of a previous iteration of this method.  <br>
11. See the results of the model in tensorboard


```python
!tensorboard --logdir=../outputs/fourthtest/
```

    TensorFlow installation not found - running with reduced feature set.
    
    NOTE: Using experimental fast data loading logic. To disable, pass
        "--load_fast=false" and report issues on GitHub. More details:
        https://github.com/tensorflow/tensorboard/issues/4784
    
    Serving TensorBoard on localhost; to expose to the network, use a proxy or pass --bind_all
    TensorBoard 2.11.0 at http://localhost:6006/ (Press CTRL+C to quit)


Some metrics


![Screenshot%202023-08-05%20at%2007-31-44%20TensorBoard.png](attachment:Screenshot%202023-08-05%20at%2007-31-44%20TensorBoard.png)

|   AP   |  AP50  |  AP75  |  APs  |  APm   |  APl   |
|:------:|:------:|:------:|:-----:|:------:|:------:|
| 30.969 | 73.927 | 22.484 | 0.000 | 21.381 | 30.098 |
[06/18 19:12:07] d2.evaluation.coco_evaluation INFO: Per-category bbox AP: 
| category   | AP     | category   | AP     | category   | AP     |
|:-----------|:-------|:-----------|:-------|:-----------|:-------|
| Author     | 22.473 | Date       | 29.107 | Page       | 13.324 |
| Text       | 63.920 | Title      | 26.023 |            |        |

### 4. Using the model to create csvs by volume.
This step can be done using a CPU instead of a GPU

run ocr_fourth.sh

This creates a list of .csv files containing the information for each volume.

Example of a layout by the model

![Picture1.png](attachment:Picture1.png)


### 5. Creating a database. 

The file pag.R compiles the database on a single file. The main objective is to use regular expressions to find text breaks that the layout model does not identify. As for our purposes, we prefer stricter definitions of contextual information; we introduce a variable named subtitle to compile extracts in the same general title. In any case, this code can be modified in cases where the journals use more extensive articles and fewer small vignettes with news. 

### 6. Testing final result. 

As a result of the database, we created a randomized sample at 95% significance (385 observations) that was tested by a human eye against the original document's layout. Here are the results


| Item         | Errors|Mistakes| Accuracy |   |
|--------------|-------|--------|----------|---|
| Type         | 7     | 1.82%  | 98.18%   |   |
| Page         | 73    | 18.96% | 81.04%   |   |
| Date         | 2     | 0.52%  | 99.48%   |   |
| Author       | 12    | 3.12%  | 96.88%   |   |
| Title        | 35    | 9.09%  | 90.91%   |   |
| Subtitle     | 55    | 14.29% | 85.71%   |   |
| Text (under) | 12    | 3.12%  |  94.81%  |   |
| Text (over)  | 8     | 2.08%  |          |   |
| Overall      | 204   | 6.62%  | 93.38%   |   |


The results show that the categories with the most mistakes are Page and Subtitle, while the ones with fewer errors are Type and Date. We divided the mistakes on the Text into two categories, one counting the times when the cell did not capture the whole piece of information (under) and a second one measuring the observation when the cell captured data from a different title. As shown in the results, the code privileged the reduction of errors that mixed information. The accuracy of the Text category is 94.81%, and the overall accuracy of the dataset is 93.38%. 
