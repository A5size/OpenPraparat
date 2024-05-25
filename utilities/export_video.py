import os
import glob
import sys
import cv2


def main():

    if os.name == 'nt':
        sep = "\\"
    elif os.name == 'posix':
        sep = "/"

    args = sys.argv
    if len(args)!=1:
        every = int(args[1])
    else:
        every = 1
        
    fourcc = cv2.VideoWriter_fourcc('m', 'p', '4', 'v')
    #fourcc = cv2.VideoWriter_fourcc(*'x264')

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

        count += 1

        if (count-1)%every==0:
            video.write(img)
            print(count, num, True)
        else:
            print(count, num, False)
        
    video.release()
    print('Exporting is complete.')


if __name__=="__main__":
    main()
    
