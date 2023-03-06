import numpy as np 
import copy
import glob

import matplotlib.pyplot as plt

def main():
    target = "p1"
    
    key_list = ["WORLD_STEP",
                "CALC_CLL-INFO_TRANS", 
                "ALONE_COUNT",
                "EAT_COUNT",
                "FUSION_COUNT",
                "CONN_CLL",
                 "EVERY_STEP_COST_A",
                "MUTATION_COUNT",
                "EXPANSION_COUNT",
                "TOT_ENERGY", 
                "CHANGE_L_COUNT",
                "ENERGY_TRANSIT_COUNT"]
    
    setting_dic = {"WORLD_STEP":
                   {"type":"int", "coeff":1.0, "plot":False},
                   "CALC_CLL-INFO_TRANS":
                   {"type":"int", "coeff":1.0, "plot":True}, 
                   "ALONE_COUNT":
                   {"type":"int", "coeff":1.0/1000.0, "plot":True},
                   "EAT_COUNT":
                   {"type":"int", "coeff":1.0/1000.0, "plot":True},
                   "FUSION_COUNT":
                   {"type":"int", "coeff":1.0/1000.0, "plot":True},
                   "CONN_CLL":
                   {"type":"int", "coeff":1.0, "plot":True},
                   "EVERY_STEP_COST_A":
                   {"type":"float", "coeff":1000.0, "plot":True},
                   "MUTATION_COUNT":
                   {"type":"int", "coeff":1.0/1000.0, "plot":True},
                   "EXPANSION_COUNT":
                   {"type":"int", "coeff":1.0/1000.0, "plot":True},
                   "TOT_ENERGY":
                   {"type":"float", "coeff":1.0/1000.0, "plot":False}, 
                   "CHANGE_L_COUNT":
                   {"type":"int", "coeff":1.0/1000.0, "plot":True},
                   "ENERGY_TRANSIT_COUNT":
                   {"type":"int", "coeff":1.0/1000.0, "plot":True}}
    
    plot_list = ["CALC_CLL-INFO_TRANS", 
                 "ALONE_COUNT",
                 "EAT_COUNT",
                 "FUSION_COUNT",
                 "CONN_CLL",
                 "EVERY_STEP_COST_A",
                 "MUTATION_COUNT",
                 "EXPANSION_COUNT",
                 "CHANGE_L_COUNT",
                 "ENERGY_TRANSIT_COUNT"]
    
    p_dic = {}
    for key in key_list:
        p_dic[key] = []
    
    out_list = []
    for out_file in glob.glob("./*dev_{}/out.o".format(target)):
        n = int(out_file.split("/")[1].replace("dev_{}".format(target), ""))
        out_list.append([n, out_file])
    out_list.sort()
    out_list = [ out_file for n, out_file in out_list ]
    
    WORLD_STEP_EP = 0
    for out_file in out_list:
        read_flag = False
        for line in open(out_file):
            if "####" in line:
                read_flag = True
            if not read_flag:
                continue
            line_array = line.split()
            tag = line_array[0]
            for key in key_list:
                if key in tag:
                    if setting_dic[key]["type"]=="int":
                        n = int(line_array[2])
                    elif setting_dic[key]["type"]=="float":
                        n = float(line_array[2])
                    if key=="WORLD_STEP":
                        n = WORLD_STEP_EP + n
                    p_dic[key].append(n)
                    #print(key, n)
                    break
        WORLD_STEP_EP = p_dic["WORLD_STEP"][-1]
    
    LEN_MIN = int(1e+17)
    for key in key_list:
        l = len(p_dic[key])
        if l<LEN_MIN:
            LEN_MIN = l
        p_dic[key] = np.array(p_dic[key])
        
        
    plt.title("Title")
    for key in key_list:
        if setting_dic[key]["plot"]:
            coeff = setting_dic[key]["coeff"]
            if coeff==1.0:
                label = key
            else:
                label = "{}*{}".format(coeff, key)
            plt.plot(p_dic["WORLD_STEP"][:LEN_MIN], coeff*p_dic[key][:LEN_MIN], label=label)
                
    plt.legend()
    plt.show()

if __name__=="__main__":
    main()
    
    
