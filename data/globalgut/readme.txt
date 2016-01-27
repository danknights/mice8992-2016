# global_gut_200k.fna: subsampled sequences from global gut study
# map.txt: simplified metadata from global gut study
# otutable.biom: closed-reference OTU table from global gut study

# to run core QIIME analyses:

# pick de novo OTUs 
time pick_de_novo_otus.py -i global_gut_200k.fna -o otus_de_novo

# run core QIIME diversity analyses on de novo OTU table
time core_diversity_analyses.py -i otus_de_novo/otu_table.biom -m map.txt -o corediv_dn -e 1000 --tree_fp otus_de_novo/rep_set.tre

# pick closed reference OTUs
time pick_closed_reference_otus.py -i global_gut_200k.fna -o otus_closed_ref
cp otus_closed_ref/otu_table.biom otu_table.biom

# run core QIIME diversity analyses on closed-reference OTU table
time core_diversity_analyses.py -i otus_closed_ref/otu_table.biom -m map.txt -o corediv_cr -e 1000 --tree_fp otus_closed_ref/97_otus.tree

# make a JSON-formatted OTU table for loading into R
biom convert -i otu_table.biom -o otu_table_json.biom --to-json

