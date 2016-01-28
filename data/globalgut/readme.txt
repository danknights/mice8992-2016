# pick de novo OTUs 
time pick_de_novo_otus.py -i seqs.fna -o denovo

# run core QIIME diversity analyses on de novo OTU table
time core_diversity_analyses.py -i denovo/otu_table.biom -m map.txt -o denovo/corediv -e 500 --tree_fp denovo/rep_set.tre

# pick closed reference OTUs
time pick_closed_reference_otus.py -i seqs.fna -o closedref

# run core QIIME diversity analyses on closed-reference OTU table
time core_diversity_analyses.py -i closedref/otu_table.biom -m map.txt -o closedref/corediv -e 500 --tree_fp closedref/97_otus.tree

# run ninja
ninja -i seqs_1m.fna -o ninja

# filter empty OTUs
biom convert -i ninja/ninja_otutable.biom --to-hdf5 -o ninja/ninja_otutable_hdf5.biom
filter_otus_from_otu_table.py -n 1 -i ninja/ninja_otutable_hdf5.biom -o otu_table.biom
biom convert -i otu_table.biom -o otu_table_json.biom --to-json
