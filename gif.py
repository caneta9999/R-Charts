import os
from random import shuffle
from PIL import Image
import imageio

n_imgs = 1 + 7
duration = 3000
dimensions = (640,320)

directory = '.\Charts'
charts = os.listdir(directory)
removeList = ['charts.gif']
for item in removeList:
    if item in charts:
        charts.remove(item)
shuffle(charts)
charts = charts[:n_imgs]
images = []
gif_directory = f'{directory}\charts.gif'

for chart in charts:
    image = Image.open(os.path.join(directory, chart))
    image = image.resize(dimensions)
    image = image.convert("RGB")
    images.append(image)

imageio.mimsave(gif_directory, images, duration=duration, loop=0)

