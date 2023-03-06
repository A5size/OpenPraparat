import numpy as np
import os
import copy
import glob
import tarfile
import f90nml

import shutil
import subprocess

import configparser

import matplotlib.pyplot as plt

class Cell(object):
    def __init__(self):
        pass

    def read_from_file(self, file_name, NOCC, NOIU, NOHU, NOOU):
        f = open(file_name)
        lines = f.readlines()
        self.read_from_array(lines[1:], NOCC, NOIU, NOHU, NOOU)

    def read_from_array(self, lines, NOCC, NOIU, NOHU, NOOU):
        self.NOCC = NOCC
        self.NOIU = NOIU
        self.NOHU = NOHU
        self.NOOU = NOOU

        in_data  = []
        out_data = []
        w1_1d    = []
        w2_1d    = []
        kocc     = []
        locc     = []
        socc     = []
        ccf      = []
        iocc     = []
        uiocc    = []

        keywords =["exist", 
                   "book", 
                   "bookmarker", 
                   "bookmarker_advance", 
                   "xyz", 
                   "nxnynz", 
                   "vxvyvz", 
                   "fxfyfz", 
                   "crcgcb", 
                   "aragab", 
                   "IOLr", 
                   "IOLg", 
                   "IOLb", 
                   "m", 
                   "r", 
                   "E", 
                   "alpha", 
                   "ID",
                   "age", 
                   "in_data", 
                   "out_data", 
                   "w1", 
                   "w2", 
                   "KOCC", 
                   "LOCC", 
                   "SOCC", 
                   "CCF", 
                   "IOCC", 
                   "UIOCC", 
                   "HIT_BLOCK_AF", 
                   "HIT_CELL_AF", 
                   "SPRING_AF", 
                   "MECHANICS_AF", 
                   "EAT_AF", 
                   "FUSION_AF", 
                   "LIGHT_AF", 
                   "INFO_TRANS_F", 
                   "NEURAL_NETWORK_F", 
                   "WAIT_FOR_CONECT_F", 
                   "WAIT_FOR_CONECT_UI", 
                   "WAIT_FOR_DISCONECT_F", 
                   "ALONE_F"] 

        self.raw_data = []
        self.raw_index = {}
        #self.raw_dic = {}
        key = "exist"
        count = 0
        for i, line in enumerate(lines):
            #print(i, line.strip())
            if key=="exist":
                self.exist = int(line.strip())
                self.raw_data.append(line.strip())
                self.raw_index["exist"] = count
                count += 1
                key = "book"
                
            elif key=="book":
                self.book = str(line.strip().replace("!", ""))
                self.raw_data.append(line.strip())
                self.raw_index["book"] = count
                count += 1
                key = "bookmarker"
                
            elif key=="bookmarker":
                self.bookmarker = str(line.strip().replace("!", ""))
                self.raw_data.append(line.strip())
                self.raw_index["bookmarker"] = count
                count += 1
                key = "bookmarker_advance"
                
            elif key=="bookmarker_advance":
                self.bookmarker_advance = str(line.strip().replace("!", ""))
                self.raw_data.append(line.strip())
                self.raw_index["bookmarker_advance"] = count
                count += 1
                key = "xyz"
                
            elif key=="xyz":
                self.x, self.y, self.z = [ float(v) for v in line.split() ]
                self.raw_data.append(line.strip())
                self.raw_index["xyz"] = count
                count += 1
                key = "nxnynz"
                
            elif key=="nxnynz":
                self.nx, self.ny, self.nz = [ int(v) for v in line.split() ]
                self.raw_data.append(line.strip())
                self.raw_index["nxnynz"] = count
                count += 1
                key = "vxvyvz"
                
            elif key=="vxvyvz":
                self.vx, self.vy, self.vz = [ float(v) for v in line.split() ]
                self.raw_data.append(line.strip())
                self.raw_index["vxvyvz"] = count
                count += 1
                key = "fxfyfz"
                
            elif key=="fxfyfz":
                self.fx, self.fy, self.fz = [ float(v) for v in line.split() ]
                self.raw_data.append(line.strip())
                self.raw_index["fxfyfz"] = count
                count += 1
                key = "crcgcb"
                
            elif key=="crcgcb":
                self.cr, self.cg, self.cb = [ float(v) for v in line.split() ]
                self.raw_data.append(line.strip())
                self.raw_index["crcgcb"] = count
                count += 1
                key = "aragab"
                
            elif key=="aragab":
                self.ar, self.ag, self.ab = [ float(v) for v in line.split() ]
                self.raw_data.append(line.strip())
                self.raw_index["aragab"] = count
                count += 1
                key = "IOLr"
                
            elif key=="IOLr":
                self.IOLr = float(line.strip())
                self.raw_data.append(line.strip())
                self.raw_index["IOLr"] = count
                count += 1
                key = "IOLg"
                
            elif key=="IOLg":
                self.IOLg = float(line.strip())
                self.raw_data.append(line.strip())
                self.raw_index["IOLg"] = count
                count += 1
                key = "IOLb"
                
            elif key=="IOLb":
                self.IOLb = float(line.strip())
                self.raw_data.append(line.strip())
                self.raw_index["IOLb"] = count
                count += 1
                key = "m"
                
            elif key=="m":
                self.m = float(line.strip())
                self.raw_data.append(line.strip())
                self.raw_index["m"] = count
                count += 1
                key = "r"
                
            elif key=="r":
                self.r = float(line.strip())
                self.raw_data.append(line.strip())
                self.raw_index["r"] = count
                count += 1
                key = "E"
                
            elif key=="E":
                self.E = float(line.strip())
                self.raw_data.append(line.strip())
                self.raw_index["E"] = count
                count += 1
                key = "alpha"
                
            elif key=="alpha":
                self.alpha = float(line.strip())
                self.raw_data.append(line.strip())
                self.raw_index["alpha"] = count
                count += 1
                key = "ID"
                
            elif key=="ID":
                self.ID = int(line.strip())
                self.raw_data.append(line.strip())
                self.raw_index["ID"] = count
                count += 1
                key = "age"

            elif key=="age":
                self.age = int(line.strip())
                self.raw_data.append(line.strip())
                self.raw_index["age"] = count
                count += 1
                key = "in_data"
                
            elif key=="in_data":
                in_data += [ float(v) for v in line.split() ]
                if len(in_data)==self.NOIU:
                    self.in_data = np.array(in_data)
                    self.raw_data.append(" ".join([ str(v) for v in in_data ]))
                    self.raw_index["in_data"] = count
                    count += 1
                    key = "out_data"
                else:
                    print("ERROR in read_from_array (in_data)")
                    exit(-1)
                    
            elif key=="out_data":
                out_data += [ float(v) for v in line.split() ]
                if len(out_data)==self.NOOU:
                    self.out_data = np.array(out_data)
                    self.raw_data.append(" ".join([ str(v) for v in out_data ]))
                    self.raw_index["out_data"] = count
                    count += 1
                    key = "w1"
                else:
                    print("ERROR in read_from_array (out_data)")
                    exit(-1)
                    
            elif key=="w1":
                w1_1d += [ float(v) for v in line.split() ]
                if len(w1_1d)==self.NOHU*(self.NOIU+1):
                    self.w1 = np.array(w1_1d).reshape([(self.NOIU+1), -1]).T
                    self.raw_data.append(" ".join([ str(v) for v in w1_1d ]))
                    self.raw_index["w1"] = count
                    count += 1
                    key = "w2"
                else:
                    print("ERROR in read_from_array (w1)")
                    exit(-1)

            elif key=="w2":
                w2_1d += [ float(v) for v in line.split() ]
                if len(w2_1d)==self.NOOU*(self.NOHU+1):
                    self.w2 = np.array(w2_1d).reshape([-1, self.NOOU]).T
                    self.raw_data.append(" ".join([ str(v) for v in w2_1d ]))
                    self.raw_index["w2"] = count
                    count += 1
                    key = "KOCC"
                else:
                    print("ERROR in read_from_array (w2)")
                    exit(-1)
                    
            elif key=="KOCC":
                kocc += [ float(v) for v in line.split() ]
                if len(kocc)==self.NOCC:
                    self.KOCC = np.array(kocc)
                    self.raw_data.append(" ".join([ str(v) for v in kocc ]))
                    self.raw_index["KOCC"] = count
                    count += 1
                    key = "LOCC"
                else:
                    print("ERROR in read_from_array (KOCC)")
                    exit(-1)
                    
            elif key=="LOCC":
                locc += [ float(v) for v in line.split() ]
                if len(locc)==self.NOCC:
                    self.LOCC = np.array(locc)
                    self.raw_data.append(" ".join([ str(v) for v in locc ]))
                    self.raw_index["LOCC"] = count
                    count += 1
                    key = "SOCC"
                else:
                    print("ERROR in read_from_array (LOCC)")
                    exit(-1)

            elif key=="SOCC":
                socc += [ float(v) for v in line.split() ]
                if len(socc)==self.NOCC:
                    self.SOCC = np.array(socc)
                    self.raw_data.append(" ".join([ str(v) for v in socc ]))
                    self.raw_index["SOCC"] = count
                    count += 1
                    key = "CCF"
                else:
                    print("ERROR in read_from_array (SOCC)")
                    exit(-1)

            elif key=="CCF":
                ccf += [ int(v) for v in line.split() ]
                if len(ccf)==self.NOCC:
                    self.CCF = np.array(ccf)
                    self.raw_data.append(" ".join([ str(v) for v in ccf ]))
                    self.raw_index["CCF"] = count
                    count += 1
                    key = "IOCC"
                else:
                    print("ERROR in read_from_array (CCF)")
                    exit(-1)

            elif key=="IOCC":
                iocc += [ int(v) for v in line.split() ]
                if len(iocc)==self.NOCC:
                    self.IOCC = np.array(iocc)
                    self.raw_data.append(" ".join([ str(v) for v in iocc ]))
                    self.raw_index["IOCC"] = count
                    count += 1
                    key = "UIOCC"
                else:
                    print("ERROR in read_from_array (IOCC)")
                    exit(-1)
                    
            elif key=="UIOCC":
                uiocc += [ int(v) for v in line.split() ]
                if len(uiocc)==self.NOCC:
                    self.UIOCC = np.array(uiocc)
                    self.raw_data.append(" ".join([ str(v) for v in uiocc ]))
                    self.raw_index["UIOCC"] = count
                    count += 1
                    key = "HIT_BLOCK_AF"
                else:
                    print("ERROR in read_from_array (UIOCC)")
                    exit(-1)
                    
            elif key=="HIT_BLOCK_AF":
                self.HIT_BLOCK_AF = int(line.strip())
                self.raw_data.append(line.strip())
                self.raw_index["HIT_BLOCK_AF"] = count
                count += 1
                key = "HIT_CELL_AF"
                
            elif key=="HIT_CELL_AF":
                self.HIT_CELL_AF = int(line.strip())
                self.raw_data.append(line.strip())
                self.raw_index["HIT_CELL_AF"] = count
                count += 1
                key = "SPRING_AF"
                
            elif key=="SPRING_AF":
                self.SPRING_AF = int(line.strip())
                self.raw_data.append(line.strip())
                self.raw_index["SPRING_AF"] = count
                count += 1
                key = "MECHANICS_AF"
                
            elif key=="MECHANICS_AF":
                self.MECHANICS_AF = int(line.strip())
                self.raw_data.append(line.strip())
                self.raw_index["MECHANICS_AF"] = count
                count += 1
                key = "EAT_AF"
                
            elif key=="EAT_AF":
                self.EAT_AF = int(line.strip())
                self.raw_data.append(line.strip())
                self.raw_index["EAT_AF"] = count
                count += 1
                key = "FUSION_AF"
                
            elif key=="FUSION_AF":
                self.FUSION_AF = int(line.strip())
                self.raw_data.append(line.strip())
                self.raw_index["FUSION_AF"] = count
                count += 1
                key = "LIGHT_AF"
                
            elif key=="LIGHT_AF":
                self.LIGHT_AF = int(line.strip())
                self.raw_data.append(line.strip())
                self.raw_index["LIGHT_AF"] = count
                count += 1
                key = "INFO_TRANS_F"
                
            elif key=="INFO_TRANS_F":
                self.INFO_TRANS_F = int(line.strip())
                self.raw_data.append(line.strip())
                self.raw_index["INFO_TRANS_F"] = count
                count += 1
                key = "NEURAL_NETWORK_F"
                
            elif key=="NEURAL_NETWORK_F":
                self.NEURAL_NETWORK_F = int(line.strip())
                self.raw_data.append(line.strip())
                self.raw_index["NEURAL_NETWORK_F"] = count
                count += 1
                key = "WAIT_FOR_CONECT_F"
                
            elif key=="WAIT_FOR_CONECT_F":
                self.WAIT_FOR_CONECT_F = int(line.strip())
                self.raw_data.append(line.strip())
                self.raw_index["WAIT_FOR_CONECT_F"] = count
                count += 1
                key = "WAIT_FOR_CONECT_UI"
                
            elif key=="WAIT_FOR_CONECT_UI":
                self.WAIT_FOR_CONECT_UI = int(line.strip())
                self.raw_data.append(line.strip())
                self.raw_index["WAIT_FOR_CONECT_UI"] = count
                count += 1
                key = "WAIT_FOR_DISCONECT_F"
                
            elif key=="WAIT_FOR_DISCONECT_F":
                self.WAIT_FOR_DISCONECT_F = int(line.strip())
                self.raw_data.append(line.strip())
                self.raw_index["WAIT_FOR_DISCONECT_F"] = count
                count += 1
                key = "ALONE_F"
                
            elif key=="ALONE_F":
                self.ALONE_F = int(line.strip())
                self.raw_data.append(line.strip())
                self.raw_index["ALONE_F"] = count
                
                return(True, i)
                
    def transform(self):
        self.IOCC  = np.array([ v-1 for v in self.IOCC  if 0<v ])
        self.UIOCC = np.array([ v-1 for v in self.UIOCC if 0<v ])

    def write(self, file_name):
        f = open(file_name, "w")
        f.write("1 \n")
        for line in self.raw_data:
            f.write(line + "\n")
        f.close()

    def write_into_file(self, f):
        for line in self.raw_data:
            f.write(line + "\n")

def count_cluster(cells, calc_cl, target_g=None):
    if target_g is not None:
        tgx, tgy, tgz = target_g
    target = None
    d_min = 1.0e+17
    len_max = -1
    beta = 0.5
    found_list = []
    alone_list = []
    cc_list = []
    cc_len_list = []
    number_of_cell = len(cells)
    for cell_id in calc_cl:
        if cell_id in found_list:
            continue
        if cells[cell_id].ALONE_F==1 and cells[cell_id].INFO_TRANS_F==0:
            alone_list.append(cell_id)
            continue
        if cells[cell_id].exist==0 or cells[cell_id].ALONE_F==1 or cells[cell_id].INFO_TRANS_F==1:
            continue
        points = np.zeros(number_of_cell)
        bpoints = np.zeros(number_of_cell)
        
        points[cell_id] = 100.0
        noupdate = 0
        for i in range(1000):
            finite_list = []
            count = 0
            for j in calc_cl:
                if cells[j].ALONE_F==1 or cells[j].INFO_TRANS_F==1:
                    continue
                if 0.0<points[j]:
                    count += 1
                    finite_list.append(points[j])
                    w = points[j]
                    l = len(cells[j].IOCC)
                    bpoints[j] += points[j] - beta*w
                    dw = (beta*w)/l
                    for k in cells[j].IOCC:
                        bpoints[k] += dw

            points[:] = bpoints[:]
            bpoints[:] = 0.0

            finite_array = np.array(finite_list)
            
            print(i, np.sum(points), np.var(points), np.var(finite_array))

            if i!=0:
                if pcount==count:
                    noupdate += 1
                    if 5<noupdate:
                        break
                else:
                    noupdate = 0

            pcount = count

        s = ""
        #conected_cell = np.array([ j for j in calc_cl if 0.0<points[j] ])
        conected_cell = [ j for j in calc_cl if 0.0<points[j] ]
        cc_list.append([len(conected_cell), conected_cell])
        cc_len_list.append(len(conected_cell))
        
        gx, gy, gz = 0.0, 0.0, 0.0
        for cc in conected_cell:
            gx += cells[cc].x
            gy += cells[cc].y
            gz += cells[cc].z
            found_list.append(cc)
            s += str(cc) + " "
        print(s)

        if target_g is not None:
            gx = gx/len(conected_cell)
            gy = gy/len(conected_cell)
            gz = gz/len(conected_cell)
            d = np.sqrt((gx-tgx)**2 + (gy-tgy)**2 + (gz-tgz)**2)
            if 3<len(conected_cell) and d<d_min:
                d_min = d
                gx_min = gx
                gy_min = gy
                gz_min = gz
                target = conected_cell

    cc_len_array = np.array(cc_len_list)
    if 0<len(cc_len_list):
        print(len(cc_len_list), cc_len_array.min(), cc_len_array.max())

    return(alone_list, cc_list, target)
            
def mix4(tar_list, dest, number_of_cell, NOCC, NOSO, NOA, NOIU, NOHU, NOOU, free_cell_path):

    half = 0.0
    cell_info_len = 42
    
    for pn in range(1, 4+1):
    
        out_file = "./{}dev_p{}/cells_file".format(dest, pn)

        memo = open("./{}dev_p{}/memo".format(dest, pn), "w")
        memo.write(" 3 | 4  \n")
        memo.write("---+--- \n")
        memo.write(" 1 | 2  \n")
        memo.write("\n")
        for n, tar_file in enumerate(tar_list):
            memo.write("{0} {1} \n".format(n+1, tar_file))
        memo.close()
    
        out = open(out_file, "w")
        out.write("{} \n".format(number_of_cell))
        
        count = 2
        for tar_i, tar_file in enumerate(tar_list):
        
            cells = {}
            calc_cl = []
            tar = tarfile.open(tar_file)
            f = tar.extractfile(tar.getmembers()[0])
            cells_file = [ line.decode("utf-8") for line in f.readlines() ]
            f.close()

            cell_count = 0
            ei = 0
            while True:
                cell = Cell()
                check, ei_ = cell.read_from_array(cells_file[ei+1:ei+1+cell_info_len], NOCC, NOIU, NOHU, NOOU)
                ei += ei_ + 1
                cell_count += 1
                print(cell_count, check, ei_, ei, cell.ID)
                cell.transform()
                cells[cell.ID-1] = copy.deepcopy(cell)
                calc_cl.append(cell.ID-1)
        
                if cell_count==number_of_cell:
                    break
            
            alone_list, cc_list, target = count_cluster(cells, calc_cl, target_g=(0.0, 0.0, 0.0))
            cc_len_array = np.array([ l for l, c in cc_list ])
    
            if tar_i==0:
                alive_list = [0]
            else:
                alive_list = []
                
            id_ex_dic = {0:0, 1:1}
            for ai in alone_list:
                if ai==0:
                    continue
                #if cells[ai].y<4:
                #    continue
                if tar_i==0:
                    if cells[ai].x<half and cells[ai].z<half:
                        alive_list.append(ai)
                        id_ex_dic[ai+1] = count
                        count += 1
                elif tar_i==1:
                    if half<=cells[ai].x and cells[ai].z<half:
                        alive_list.append(ai)
                        id_ex_dic[ai+1] = count
                        count += 1
                elif tar_i==2:
                    if cells[ai].x<half and half<=cells[ai].z:
                        alive_list.append(ai)
                        id_ex_dic[ai+1] = count
                        count += 1
                elif tar_i==3:
                    if half<=cells[ai].x and half<=cells[ai].z:
                        alive_list.append(ai)
                        id_ex_dic[ai+1] = count
                        count += 1
        
            for l, cl in cc_list:
                flag = True
                for i in cl:
                    if tar_i==0:
                        if half<=cells[i].x or half<=cells[i].z:
                            flag = False
                            break
                    elif tar_i==1:
                        if cells[i].x<half or half<=cells[i].z:
                            flag = False
                            break
                    elif tar_i==2:
                        if half<=cells[i].x or cells[i].z<half:
                            flag = False
                            break
                    elif tar_i==3:
                        if cells[i].x<half or cells[i].z<half:
                            flag = False
                            break
                        
                if flag:
                    alive_list += cl
                    for i in cl:
                        id_ex_dic[i+1] = count
                        count += 1      
            
            print("alive_list=", alive_list)
            for ai in alive_list:
                cells[ai].raw_data[cells[ai].raw_index["ID"]] = str(id_ex_dic[ai+1])
                if cells[ai].ALONE_F!=1:
                    iocc_list = [ int(iocc) for iocc in cells[ai].raw_data[cells[ai].raw_index["IOCC"]].split() ]
                    cells[ai].raw_data[cells[ai].raw_index["IOCC"]] = " ".join([ str(id_ex_dic[iocc]) for iocc in iocc_list ])
    
                cells[ai].write_into_file(out)
        
            free_cell = Cell()
            free_cell.read_from_file("{}/free_cell".format(free_cell_path), NOCC, NOIU, NOHU, NOOU)
    
        for cell_id in range(count, number_of_cell+1):
            free_cell.raw_data[free_cell.raw_index["ID"]] = str(cell_id)
            free_cell.write_into_file(out)
            
        out.close()

        tar_list = tar_list[1:] + [tar_list[0]]

        
def read_out(path):
    key_list = ["WORLD_STEP"             ,
                "CALC_CLL"               ,
                "NOT_CALC_CLL"           , 
                "NUMBER_OF_INFO_TRANS"   , 
                "CALC_CLL-INFO_TRANS"    , 
                "CALC_CLL+NOT_CALC_CLL"  , 
                "CONN_CLL"               , 
                "EAT_COUNT*"             , 
                "EAT_INFO*"              , 
                "EATEN_INFO*"            , 
                "FUSION_COUNT*"          , 
                "EXPANSION_COUNT*"       , 
                "CONNECT_COUNT*"         , 
                "DISCONNECT_COUNT*"      , 
                "TURN_BOOKMARKER_COUNT*" , 
                "MUTATION_COUNT*"        , 
                "DEATHS_COUNT*"          , 
                "CELLS(SUN_ID)%LIGHT_AF" , 
                "sun_theta"              , 
                "TOT_ENERGY*"            , 
                "AVE_ENERGY*"            , 
                "EVERY_STEP_COST_A"      , 
                "CHANGE_L_COUNT*"        , 
                "ENERGY_TRANSIT_COUNT*"  , 
                "CENTER_OF_GRAV"         , 
                "AGE INFO FOR CELL"      , 
                "AGE INFO FOR IT"        , 
                "TOT_M*"                 , 
                "ALONE_COUNT*"           ] 

    setting_dic = {"WORLD_STEP"             :{"type":int},
                   "CALC_CLL"               :{"type":int},
                   "NOT_CALC_CLL"           :{"type":int}, 
                   "NUMBER_OF_INFO_TRANS"   :{"type":int}, 
                   "CALC_CLL-INFO_TRANS"    :{"type":int}, 
                   "CALC_CLL+NOT_CALC_CLL"  :{"type":int}, 
                   "CONN_CLL"               :{"type":int}, 
                   "EAT_COUNT*"             :{"type":int}, 
                   "EAT_INFO*"              :{"type":list, "c_type":[int]*(8+1)}, 
                   "EATEN_INFO*"            :{"type":list, "c_type":[int]*(8+1)}, 
                   "FUSION_COUNT*"          :{"type":int}, 
                   "EXPANSION_COUNT*"       :{"type":int}, 
                   "CONNECT_COUNT*"         :{"type":int}, 
                   "DISCONNECT_COUNT*"      :{"type":int}, 
                   "TURN_BOOKMARKER_COUNT*" :{"type":int}, 
                   "MUTATION_COUNT*"        :{"type":int}, 
                   "DEATHS_COUNT*"          :{"type":int}, 
                   "CELLS(SUN_ID)%LIGHT_AF" :{"type":list, "c_type":[int]*3}, 
                   "sun_theta"              :{"type":float}, 
                   "TOT_ENERGY*"            :{"type":float}, 
                   "AVE_ENERGY*"            :{"type":float}, 
                   "EVERY_STEP_COST_A"      :{"type":float}, 
                   "CHANGE_L_COUNT*"        :{"type":int}, 
                   "ENERGY_TRANSIT_COUNT*"  :{"type":int}, 
                   "CENTER_OF_GRAV"         :{"type":list, "c_type":[float]*3}, 
                   "AGE INFO FOR CELL"      :{"type":list, "c_type":[float, float, int, int]}, 
                   "AGE INFO FOR IT"        :{"type":list, "c_type":[float, float, int, int]}, 
                   "TOT_M*"                 :{"type":float}, 
                   "ALONE_COUNT*"           :{"type":int}}

    data_dic = { key:[] for key in key_list }
    read_flag = False
    for line in open(path):
        if "#####################################################" in line:
            read_flag = True
            continue
        if not read_flag:
            continue
        if not "=" in line:
            continue
        tag, value = line.split("=")
        tag = tag.strip()
        for key in key_list:
            #print(key, tag, key==tag)
            if key==tag:
                if   setting_dic[key]["type"] is int:
                    try:
                        value = int(value)
                    except:
                        value = -1
                    data_dic[key].append(value)
                elif setting_dic[key]["type"] is float:
                    try:
                        value = float(value)
                    except:
                        value = -1
                    data_dic[key].append(value)
                elif setting_dic[key]["type"] is list:
                    _ = []
                    for t, v in zip(setting_dic[key]["c_type"], value.split()):
                        try:
                            v = t(v)
                        except:
                            v = t(-1)
                        _.append(v)
                    #_ = [ t(v) for t, v in zip(setting_dic[key]["c_type"], value.split()) ]
                    data_dic[key].append(_)
                break

    len_min = np.min([ len(v) for k, v in data_dic.items() ])
    step_dic = {}
    for i in range(len_min):
        step_dic[data_dic["WORLD_STEP"][i]] = { key:data_dic[key][i] for key in key_list } 

    return(data_dic, step_dic)


def main():

    config_ini = configparser.ConfigParser()
    config_ini.read('./config.ini', encoding='utf-8')

    path = config_ini['Environment']['path']
    
    praparat_deb = "{}/praparat_deb".format(path)
    praparat     = "{}/praparat_cui".format(path)

    if not os.path.exists(praparat_deb):
        praparat_deb = "{}/praparat_deb.exe".format(path)
        if not os.path.exists(praparat_deb):
            print("ERROR: praparat_deb not found. ")
            exit(-1)
            
    if not os.path.exists(praparat):
        praparat = "{}/praparat.exe".format(path)
        if not os.path.exists(praparat):
            print("ERROR: praparat not found. ")
            exit(-1)

    number_of_cell = int(config_ini['Cell']['number_of_cell'])

    NOCC = int(config_ini['Cell']['NOCC'])
    NOSO = int(config_ini['Cell']['NOSO'])
    NOA  = int(config_ini['Cell']['NOA'])
    NOHU = int(config_ini['Cell']['NOHU']) # Number Of Hidden Units
    
    MAX_UPDATE     = int(config_ini['Settings']['MAX_UPDATE']) 
    deb_limit      = int(config_ini['Settings']['deb_limit'])
    ini_cells_file = config_ini['Settings']['ini_cells_file']

    
    NOIU = 3*NOCC + NOSO  # Number Of Input Units
    NOOU = 3*NOCC + NOA   # Number Of Output Units

    
    cwd = os.getcwd()
    
    input_path = "./inputs"
    ini_path = "./001ini"
    
    if not os.path.exists(ini_path):
        os.mkdir(ini_path)
        shutil.copyfile("{}/input0".format(input_path),  "{}/input".format(ini_path))
        shutil.copyfile("{}/{}".format(input_path, ini_cells_file), "{}/cells_file".format(ini_path))
        shutil.copyfile("{}/field.d".format(input_path), "{}/field.d".format(ini_path))
        os.chdir(ini_path)
        fo = open("./out.o", "w")
        proc = subprocess.Popen([praparat_deb], stdout=fo)
        os.chdir(cwd)
        proc.wait()
        fo.close()

    if len(glob.glob("./002dev_p*"))==0:
        input0 = f90nml.read("{}/input0".format(input_path))
        out_list = []
        proc_list = []
        for i in range(1, 4+1):
            dest_path = "./002dev_p{}".format(i)
            os.mkdir(dest_path)
            shutil.copyfile("{}/input{}".format(input_path, i), "{}/input".format(dest_path))
            shutil.copyfile("{}/cells{}.dat.tar.gz".format(ini_path, input0["cycle"]["out_cycle"]),
                            "{}/cells{}.dat.tar.gz".format(dest_path, input0["cycle"]["out_cycle"]))
            data_dic, step_dic = read_out("{}/out.o".format(ini_path))
            ave_a = np.mean(data_dic["EVERY_STEP_COST_A"][len(data_dic["EVERY_STEP_COST_A"])//2:])
            os.chdir(dest_path)
            with open("./input", "r") as f:
                input_ = f.read()
            input_ = input_.replace("__EVERY_STEP_COST_A__", "{:12.8f}d0".format(ave_a))
            with open("./input", "w") as f:
                f.write(input_)
            subprocess.call(['tar', "-zxvf", "cells{}.dat.tar.gz".format(input0["cycle"]["out_cycle"])])
            subprocess.call(['rm', "cells{}.dat.tar.gz".format(input0["cycle"]["out_cycle"])])
            subprocess.call(['mv', "cells{}.dat".format(input0["cycle"]["out_cycle"]), "cells_file"])
            subprocess.call(['ln', "-s", "../{}/field.d".format(ini_path), "."])
            out_list.append(open("./out.o", "w"))
            proc = subprocess.Popen([praparat_deb], stdout=out_list[-1])
            proc_list.append(proc)
            os.chdir(cwd)
     
        for proc in proc_list:
            proc.wait()
     
        for fo in out_list:
            fo.close()
    elif 0<len(glob.glob("./002dev_p*"))<4:
        print("002 is halfway through the process.")
        print("002 should be removed.")
        exit(-1)
            

    input1 = f90nml.read("{}/input1".format(input_path))
    input2 = f90nml.read("{}/input2".format(input_path))
    input3 = f90nml.read("{}/input3".format(input_path))
    input4 = f90nml.read("{}/input4".format(input_path))

    input1_oc = input1["cycle"]["out_cycle"]
    input2_oc = input2["cycle"]["out_cycle"]
    input3_oc = input3["cycle"]["out_cycle"]
    input4_oc = input4["cycle"]["out_cycle"]
        
    for n in range(3, MAX_UPDATE):
        if  len(glob.glob("./{:0>3}dev_p*".format(n)))==4:
            continue
        elif 0<len(glob.glob("./{:0>3}dev_p*".format(n)))<4:
            print("{:0>3} is halfway through the process.".format(n))
            print("{:0>3} should be removed.".format(n))
            exit(-1)
        
        for i in range(1, 4+1):
            dest_path = "./{:0>3}dev_p{}".format(n, i)
            os.mkdir(dest_path)
            
        tar_list = ["./{:0>3}dev_p1/cells{}.dat.tar.gz".format(n-1, input1_oc),
                    "./{:0>3}dev_p2/cells{}.dat.tar.gz".format(n-1, input2_oc),
                    "./{:0>3}dev_p3/cells{}.dat.tar.gz".format(n-1, input3_oc),
                    "./{:0>3}dev_p4/cells{}.dat.tar.gz".format(n-1, input4_oc)]
        
        mix4(tar_list, "{:0>3}".format(n), number_of_cell, NOCC, NOSO, NOA, NOIU, NOHU, NOOU, input_path)

        out_list = []
        proc_list = []
        for i in range(1, 4+1):
            src_path  = "./{:0>3}dev_p{}".format(n-1, i)
            dest_path = "./{:0>3}dev_p{}".format(n, i)
            shutil.copyfile("{}/input{}".format(input_path, i), "{}/input".format(dest_path))
            data_dic, step_dic = read_out("{}/out.o".format(src_path))
            ave_a = np.mean(data_dic["EVERY_STEP_COST_A"][len(data_dic["EVERY_STEP_COST_A"])//2:])
            os.chdir(dest_path)
            with open("./input", "r") as f:
                input_ = f.read()
            input_ = input_.replace("__EVERY_STEP_COST_A__", "{:12.8f}d0".format(ave_a))
            with open("./input", "w") as f:
                f.write(input_)
            subprocess.call(['ln', "-s", "../{}/field.d".format(ini_path), "."])
            out_list.append(open("./out.o", "w"))
            if n<=deb_limit:
                proc = subprocess.Popen([praparat_deb], stdout=out_list[-1])
            else:
                proc = subprocess.Popen([praparat], stdout=out_list[-1])
            proc_list.append(proc)
            os.chdir(cwd)
            
        for proc in proc_list:
            proc.wait()

        for fo in out_list:
            fo.close()

        
if __name__=="__main__":
    main()

