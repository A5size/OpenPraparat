import os
import glob
import sys
import cv2


def main():

    if os.name == 'nt':
        sep = "\\"
    elif os.name == 'posix':
        sep = "/"
    
    fourcc = cv2.VideoWriter_fourcc('m', 'p', '4', 'v')

    img = cv2.imread(glob.glob(".{0}pic{0}*".format(sep))[0])
    height, width, channels = img.shape[:3]

    video = cv2.VideoWriter('.{}video.mp4'.format(sep), fourcc, 60.0, (width, height))
    
    if not video.isOpened():
        print("Open failed.")
        exit(-1)

    file_list = [ [int(fn.split(sep)[-1].replace(".bmp", "")), fn] for fn in glob.glob(".{0}pic{0}*".format(sep)) ]
    file_list.sort()

    count = 0
    for num, file_name in file_list:
        
        img = cv2.imread(file_name)
        if img is None:
            print("{} cannot be read.".format(file_name))
            break

        video.write(img)
        count += 1
        
        print(count, num)
        
    video.release()
    print('Exporting is complete.')


if __name__=="__main__":
    main()
    
