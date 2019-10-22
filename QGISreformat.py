#VIC_instruct.csv
#follow the instruction to reformat the QGIS generated csv file
import csv
def combine_rows(row_gro):
    #to combine rows with same ID, return one row
    row_gro2 = row_gro[0]
    j = 1
    while j <= len(row_gro):
        for attri in range(4, 21):
            if row_gro[j - 1][attri] == "Y":
                row_gro2[attri] = "Y"
        j = j + 1
    return row_gro2

def write_csv_line(good_line,wr):
    #write row to out files
    header = ['id','LATITUDEDD_NUM','LONGITUDEDD_NUM','FTYPE_CODE',
              'bay','bua','forest','island','island_marine','lga',
              'locality','mainland','named_natural_region','postcode',
              'public_land','ridge','sea','vicgov_region','wb_lake',
              'wb_lake_salt','wetland_swamp' ]
    if good_line == "000000000000000000000":
        wr.writerow(header)#17
        #write the header
    else:
        wr.writerow(good_line)
        # write the good row

def process_rows(filename,output_name):
    #main part of process, read, combine and write
    with open(filename, "r") as f:
        reader = csv.reader(f, delimiter="\t")
        curr_id = 'id'
        row_gro = ['0'*21]
        with open(output_name, "w", newline='') as fp:
            wr = csv.writer(fp, dialect='excel')
            for i, line in enumerate(reader):
                curr_line = line[0].split(",")
                if i != 0:
                    # print ('line[{}] = {}'.format(i, line))
                    if curr_line[0] != curr_id:
                        good_line = combine_rows(row_gro)
                        curr_id = curr_line[0]
                        row_gro = []
                        write_csv_line(good_line,wr)#write good_lines to csv file
                        row_gro.append(curr_line)
                    elif curr_line[0] == curr_id:
                        row_gro.append(curr_line)
            good_line = combine_rows(row_gro)
            write_csv_line(good_line,wr)

process_rows("VIC_instruct.csv","OUT_instruct.csv")