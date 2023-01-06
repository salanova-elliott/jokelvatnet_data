#!/usr/bin/env python

import sys
import pprint

'''
Outputs arriving and departing taxa for an arbitrary number of clusters input.
Numbers are boundaries between clusters.

Usage:

    python cluster_taxa.py All_taxa_age.tsv 200 600 8500 5500

'''

ages = set()
age_dict = {}

with open(sys.argv[1]) as f:

    headers = f.readline().rstrip().split("\t")[1:]

    for l in f:

        l_list = l.rstrip().split("\t")

        ages.add(int(l_list[0]))
        age_dict[int(l_list[0])] = {}

        for i, num in enumerate(l_list[1:]):
            age_dict[int(l_list[0])][headers[i]] = num

divs = [int(item) for item in sys.argv[2:]]
divs.sort()

cluster_ages = []

for d in divs:
    temp_l = []
    for a in ages:
        if a < d:
            temp_l.append(a)
    if temp_l:
        ages = ages.difference(set(temp_l))
        cluster_ages.append(sorted(temp_l))
if ages:
    cluster_ages.append(sorted(list(ages)))

# Makes lists of species present in each cluster
nonexclu_clusters = [set() for _ in range(len(cluster_ages))]
for i, cluster in enumerate(cluster_ages[::-1]):
    for age in cluster:
        for taxa in age_dict[age]:
            if float(age_dict[age][taxa]) > 0:
                nonexclu_clusters[i].add(taxa)

#pprint.pprint(nonexclu_clusters)

for i, cluster in enumerate(nonexclu_clusters):

    # First cluster
    if i == 0:
        print(f"{cluster_ages[-1][-1]}\t{cluster_ages[-1][0]}")
        print(f"Starting taxa {len(cluster)}\n")
        for taxa in sorted(list(cluster)):
            print(taxa)
        print("\n")

    # Last cluster
    #elif i == (len(nonexclu_clusters) - 1):
    #    print(f"{cluster_ages[0][-1]}\t{cluster_ages[0][0]}")
    #    print("Arriving taxa\n")
    #    for taxa in sorted(list(cluster - nonexclu_clusters[i-1])):
    #        print(taxa)

    # In between clusters
    else:
        print(f"{cluster_ages[-i-1][-1]}\t{cluster_ages[-i-1][0]}")
        print(f"Arriving taxa {len(cluster - nonexclu_clusters[i-1])}\n")
        for taxa in sorted(list(cluster - nonexclu_clusters[i-1])):
            print(taxa)
        print(f"\nDeparting taxa {len(nonexclu_clusters[i-1] - cluster)}\n")
        for taxa in sorted(list(nonexclu_clusters[i-1] - cluster)):
            print(taxa)
        print("\n")
'''
print(f"{len(nonexclu_clusters[0])} {len(nonexclu_clusters[1])} {len(nonexclu_clusters[2])}")

print(f"Z1,2\n{len(nonexclu_clusters[0].intersection(nonexclu_clusters[1]).difference(nonexclu_clusters[2]))}")
print(f"Z1,3\n{len(nonexclu_clusters[0].intersection(nonexclu_clusters[2]).difference(nonexclu_clusters[1]))}")
print(f"Z2,3\n{len(nonexclu_clusters[1].intersection(nonexclu_clusters[2]).difference(nonexclu_clusters[0]))}")

print(f"Z1,2,3\n{len(nonexclu_clusters[0].intersection(nonexclu_clusters[1],nonexclu_clusters[2]))}")

print(f"Z1\n{len(nonexclu_clusters[0].difference(nonexclu_clusters[1],nonexclu_clusters[2]))}\n{nonexclu_clusters[0].difference(nonexclu_clusters[1],nonexclu_clusters[2])}")
print(f"Z2\n{len(nonexclu_clusters[1].difference(nonexclu_clusters[0],nonexclu_clusters[2]))}")
print(f"Z3\n{len(nonexclu_clusters[2].difference(nonexclu_clusters[0],nonexclu_clusters[1]))}")
'''
'''
print(f"Z1,2\n{len(nonexclu_clusters[0].intersection(nonexclu_clusters[1]).difference(nonexclu_clusters[2],nonexclu_clusters[3]))}")
print(f"Z1,3\n{len(nonexclu_clusters[0].intersection(nonexclu_clusters[2]).difference(nonexclu_clusters[1],nonexclu_clusters[3]))}")
print(f"Z2,3\n{len(nonexclu_clusters[1].intersection(nonexclu_clusters[2]).difference(nonexclu_clusters[0],nonexclu_clusters[3]))}")

print(f"Z1,4\n{len(nonexclu_clusters[0].intersection(nonexclu_clusters[3]).difference(nonexclu_clusters[1],nonexclu_clusters[2]))}")
print(f"Z2,4\n{len(nonexclu_clusters[1].intersection(nonexclu_clusters[3]).difference(nonexclu_clusters[0],nonexclu_clusters[2]))}")
print(f"Z3,4\n{len(nonexclu_clusters[2].intersection(nonexclu_clusters[3]).difference(nonexclu_clusters[0],nonexclu_clusters[1]))}")

print(f"Z1,2,3\n{len(nonexclu_clusters[0].intersection(nonexclu_clusters[1],nonexclu_clusters[2]).difference(nonexclu_clusters[3]))}")
print(f"Z1,2,4\n{len(nonexclu_clusters[0].intersection(nonexclu_clusters[1],nonexclu_clusters[3]).difference(nonexclu_clusters[2]))}")
print(f"Z1,3,4\n{len(nonexclu_clusters[0].intersection(nonexclu_clusters[2],nonexclu_clusters[3]).difference(nonexclu_clusters[1]))}")
print(f"Z2,3,4\n{len(nonexclu_clusters[1].intersection(nonexclu_clusters[2],nonexclu_clusters[3]).difference(nonexclu_clusters[0]))}")

print(f"Z1,2,3,4\n{len(nonexclu_clusters[0].intersection(nonexclu_clusters[1],nonexclu_clusters[2],nonexclu_clusters[3]))}")

print(f"Z1\n{len(nonexclu_clusters[0].difference(nonexclu_clusters[1],nonexclu_clusters[2],nonexclu_clusters[3]))}")
print(f"{nonexclu_clusters[0].difference(nonexclu_clusters[1],nonexclu_clusters[2],nonexclu_clusters[3])}\n")

print(f"Z2\n{len(nonexclu_clusters[1].difference(nonexclu_clusters[0],nonexclu_clusters[2],nonexclu_clusters[3]))}")
print(f"{nonexclu_clusters[1].difference(nonexclu_clusters[0],nonexclu_clusters[2],nonexclu_clusters[3])}\n")

print(f"Z3\n{len(nonexclu_clusters[2].difference(nonexclu_clusters[0],nonexclu_clusters[1],nonexclu_clusters[3]))}")
print(f"Z4\n{len(nonexclu_clusters[3].difference(nonexclu_clusters[0],nonexclu_clusters[1],nonexclu_clusters[2]))}")
'''

print(f"Z1 nonexclu\n{len(nonexclu_clusters[0])}")
print(f"Z2 nonexclu\n{len(nonexclu_clusters[1])}")
print(f"Z3 nonexclu\n{len(nonexclu_clusters[2])}")
print(f"Z4 nonexclu\n{len(nonexclu_clusters[3])}")

print(f"Z1,2\n{len(nonexclu_clusters[0].intersection(nonexclu_clusters[1]))}")
print(f"Z1,3\n{len(nonexclu_clusters[0].intersection(nonexclu_clusters[2]))}")
print(f"Z2,3\n{len(nonexclu_clusters[1].intersection(nonexclu_clusters[2]))}")
print(f"Z1,4\n{len(nonexclu_clusters[0].intersection(nonexclu_clusters[3]))}")
print(f"Z2,4\n{len(nonexclu_clusters[1].intersection(nonexclu_clusters[3]))}")
print(f"Z3,4\n{len(nonexclu_clusters[2].intersection(nonexclu_clusters[3]))}")

print(f"Z1,2,3\n{len(nonexclu_clusters[0].intersection(nonexclu_clusters[1],nonexclu_clusters[2]))}")
print(f"Z1,2,4\n{len(nonexclu_clusters[0].intersection(nonexclu_clusters[1],nonexclu_clusters[3]))}")
print(f"Z1,3,4\n{len(nonexclu_clusters[0].intersection(nonexclu_clusters[2],nonexclu_clusters[3]))}")
print(f"Z2,3,4\n{len(nonexclu_clusters[1].intersection(nonexclu_clusters[2],nonexclu_clusters[3]))}")

print(f"Z1,2,3,4\n{len(nonexclu_clusters[0].intersection(nonexclu_clusters[1],nonexclu_clusters[2],nonexclu_clusters[3]))}")

print(f"\nZ1,4 taxa \n{(nonexclu_clusters[0].intersection(nonexclu_clusters[3]).difference(nonexclu_clusters[1],nonexclu_clusters[2]))}")
print(f"\nZ1,3,4 taxa \n{(nonexclu_clusters[0].intersection(nonexclu_clusters[3],nonexclu_clusters[2]).difference(nonexclu_clusters[1]))}")

