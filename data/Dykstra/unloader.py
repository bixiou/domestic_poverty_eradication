import os
import csv
import pdb
import time
from pymongo import MongoClient, ASCENDING, DESCENDING

working_dir = os.getcwd()
path = "{}/{}".format(working_dir, "output")
if not os.path.exists(path):
    os.mkdir(path)

mongo = MongoClient()
db = mongo.povcal

surveys = db.surveys.find()

for survey in surveys:
    path = "{}/output/{}".format(working_dir, survey["country"])
    if not os.path.exists(path):
        os.mkdir(path)
    filename = "{0}/{1} {2}-{3}.csv".format(path, survey["country"], survey["year"], survey["type"].upper())
    with open(filename, 'w') as fout:
        fout.write("Pov. Line (PPP$/mo), Headcount (%), PovcalNet Error?\n")
        rows = db.headcounts.find({"country": survey["country"], "year": survey["year"],"type": survey["type"]}).sort("pl_cents", ASCENDING)
        last_headcount = None 
        for row in rows:
            headcount = row["headcount"]
            if not headcount == last_headcount:
                last_headcount = headcount
                if isinstance(headcount, int):
                    new_line = "{}, {}, ".format(float(row["pl_cents"])/100, float(headcount)/100)
                else:
                    new_line = "{}, {}, ".format(float(row["pl_cents"])/100, 'na')
                if "bad_pl_returned" in row:
                    new_line += "true\n"
                else:
                    new_line += "false\n"

                fout.write(new_line)
