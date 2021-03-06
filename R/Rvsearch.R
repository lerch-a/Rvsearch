
## Example
# "Usage: /Library/Frameworks/R.framework/Versions/3.3/Resources/library/Rvsearch/vsearch [OPTIONS]"

# [6] "General options"                                                                                 
# [7] "  --fasta_width INT           width of FASTA seq lines, 0 for no wrap (80)"                      
# [9] "  --log FILENAME              write messages, timing and memory info to file"                    
# [10] "  --maxseqlength INT          maximum sequence length (50000)"                                   
# [11] "  --minseqlength INT          min seq length (clust/derep/search: 32, other:1)"                  
# [12] "  --notrunclabels             do not truncate labels at first space"                             
# [13] "  --quiet                     output just warnings and fatal errors to stderr"                   
# [14] "  --threads INT               number of threads to use, zero for all cores (0)"                  

## The main wrapper around vsearch

# Chimera detection using a reference database       
# chimeraDetectionRef <- function()
# {
#   # [19] "  --uchime_ref FILENAME       detect chimeras" 
#   # [25] "  --db FILENAME               reference database for --uchime_ref" 
#   # [36] "  --self                      exclude identical labels for --uchime_ref"                         
#   # [37] "  --selfid                    exclude identical sequences for --uchime_ref"    
# }

# Chimera detection de novo          
chimeraDetectionDenovo <- function(filename, abskew=2.0, mindiffs=3L, mindiv=0.8, minh=0.28,
                                   nonchimeras=sub(".fasta", "_nonchimera.fasta", filename), 
                                   chimeras=sub(".fasta", "_chimera.fasta", filename), 
                                   borderline=sub(".fasta", "_bordchimera.fasta", filename), 
                                   uchimeout=sub(".fasta", "_chimeraResults.txt", filename))
{
  
  syscall <- paste("--uchime_denovo ", filename, 
                   "--abskew", abskew,
                   "--mindiffs", mindiffs, 
                   "--mindiv", mindiv,
                   "--minh", minh, 
                   "--nonchimeras", nonchimerafile, 
                   "--chimeras", chimerafile, 
                   "--borderline", borderfile, 
                   "--uchimeout", uchimeout, sep=" ")
  # System call parameters                                                                     
  # [18] "  --uchime_denovo FILENAME    detect chimeras "                                           
  # [20] "Options"                                                                                         
  # [21] "  --abskew REAL               min abundance ratio of parent vs chimera (2.0)"                    
  # [28] "  --mindiffs INT              minimum number of differences in segment (3)"                      
  # [29] "  --mindiv REAL               minimum divergence from closest parent (0.8)"                      
  # [30] "  --minh REAL                 minimum score (0.28)"    
  
  # [23] "  --borderline FILENAME       output borderline chimeric sequences to file"                      
  # [24] "  --chimeras FILENAME         output chimeric sequences to file"  
  # [31] "  --nonchimeras FILENAME      output non-chimeric sequences to file"
  # [40] "  --uchimeout FILENAME        output to chimera info to tab-separated file" 
  
  # Not supported in this function
  # [26] "  --dn REAL                   'no' vote pseudo-count (1.4)"                                      
  # [27] "  --fasta_score               include chimera score in fasta output"   
  # [32] "  --relabel STRING            relabel nonchimeras with this prefix string"                       
  # [33] "  --relabel_keep              keep the old label after the new when relabelling"                 
  # [34] "  --relabel_md5               relabel with md5 digest of normalized sequence"                    
  # [35] "  --relabel_sha1              relabel with sha1 digest of normalized sequence"                   
  # [38] "  --sizeout                   include abundance information when relabelling"                    
  # [39] "  --uchimealns FILENAME       output chimera alignments to file"
  # [22] "  --alignwidth INT            width of alignment in uchimealn output (80)"  
  # [41] "  --uchimeout5                make output compatible with uchime version 5"                      
  # [42] "  --xn REAL                   'no' vote weight (8.0)"                                            
  # [43] "  --xsize                     strip abundance information in output"  
  
  return(invisible(.vsearchBin(args=syscall)))
}


## Helper function that return a description of the intended usage for vsearch
vsearch_usage <- function(){
  print(.vsearchBin(args="--help"))
}


## Helper function that return the version of vsearch
vsearch_version <- function(){
    print(.vsearchBin(args="--version"))
}


## A helper function to call the vsearch binaries with additional arguments.
.vsearchBin <- function(args="")
{
    if(is.null(args) || args=="")
        stop("The vsearch binaries need to be called with additional arguments")
    args <- gsub("^ *| *$", "", args)
    call <- paste(shQuote(file.path(system.file(package="Rvsearch"), "vsearch")), args)
    return(system(call, intern=TRUE))
}


## The direct binary call function
.execute <- function(callstr, ...){
  call <- file.path(shQuote(system.file(package="Rvsearch")), callstr)
  return(system(call, ...))
}


# [45] "Clustering"                                                                                      
# [46] "  --cluster_fast FILENAME     cluster sequences after sorting by length"                         
# [47] "  --cluster_size FILENAME     cluster sequences after sorting by abundance"                      
# [48] "  --cluster_smallmem FILENAME cluster already sorted sequences (see -usersort)"                  
# [49] "Options (most searching options also apply)"                                                     
# [50] "  --centroids FILENAME        output centroid sequences to FASTA file"                           
# [51] "  --clusterout_id             add cluster id info to consout and profile files"                  
# [52] "  --clusterout_sort           order msaout, consout, profile by decr abundance"                  
# [53] "  --clusters STRING           output each cluster to a separate FASTA file"                      
# [54] "  --consout FILENAME          output cluster consensus sequences to FASTA file"                  
# [55] "  --cons_truncate             do not ignore terminal gaps in MSA for consensus"                  
# [56] "  --id REAL                   reject if identity lower"                                          
# [57] "  --iddef INT                 id definition, 0-4=CD-HIT,all,int,MBL,BLAST (2)"                   
# [58] "  --msaout FILENAME           output multiple seq. alignments to FASTA file"                     
# [59] "  --profile FILENAME          output sequence profile of each cluster to file"                   
# [60] "  --qmask none|dust|soft      mask seqs with dust, soft or no method (dust)"                     
# [61] "  --relabel STRING            relabel centroids with this prefix string"                         
# [62] "  --relabel_keep              keep the old label after the new when relabelling"                 
# [63] "  --relabel_md5               relabel with md5 digest of normalized sequence"                    
# [64] "  --relabel_sha1              relabel with sha1 digest of normalized sequence"                   
# [65] "  --sizein                    propagate abundance annotation from input"                         
# [66] "  --sizeorder                 sort accepted centroids by abundance (AGC)"                        
# [67] "  --sizeout                   write cluster abundances to centroid file"                         
# [68] "  --strand plus|both          cluster using plus or both strands (plus)"                         
# [69] "  --uc FILENAME               specify filename for UCLUST-like output"                           
# [70] "  --usersort                  indicate sequences not pre-sorted by length"                       
# [71] "  --xsize                     strip abundance information in output"                             

# [73] "Dereplication and rereplication"                                                                 
# [74] "  --derep_fulllength FILENAME dereplicate sequences in the given FASTA file"                     
# [75] "  --derep_prefix FILENAME     dereplicate sequences in file based on prefixes"                   
# [76] "  --rereplicate FILENAME      rereplicate sequences in the given FASTA file"                     
# [77] "Options"                                                                                         
# [78] "  --maxuniquesize INT         maximum abundance for output from dereplication"                   
# [79] "  --minuniquesize INT         minimum abundance for output from dereplication"                   
# [80] "  --output FILENAME           output FASTA file"                                                 
# [81] "  --relabel STRING            relabel with this prefix string"                                   
# [82] "  --relabel_keep              keep the old label after the new when relabelling"                 
# [83] "  --relabel_md5               relabel with md5 digest of normalized sequence"                    
# [84] "  --relabel_sha1              relabel with sha1 digest of normalized sequence"                   
# [85] "  --sizein                    propagate abundance annotation from input"                         
# [86] "  --sizeout                   write abundance annotation to output"                              
# [87] "  --strand plus|both          dereplicate plus or both strands (plus)"                           
# [88] "  --topn INT                  output only n most abundant sequences after derep"                 
# [89] "  --uc FILENAME               filename for UCLUST-like dereplication output"                     
# [90] "  --xsize                     strip abundance information in derep output"                       

# [92] "FASTQ filtering"                                                                                 
# [93] "  --fastq_filter FILENAME     filter FASTQ file, output to FASTQ or FASTA file"                  
# [94] "Options"                                                                                         
# [95] "  --eeout                     include expected errors in FASTQ filter output"                    
# [96] "  --fastaout FILENAME         FASTA output filename for passed sequences"                        
# [97] "  --fastaout_discarded FNAME  FASTA filename for discarded sequences"                            
# [98] "  --fastqout FILENAME         FASTQ output filename for passed sequences"                        
# [99] "  --fastqout_discarded FNAME  FASTQ filename for discarded sequences"                            
# [100] "  --fastq_ascii INT           FASTQ input quality score ASCII base char (33)"                    
# [101] "  --fastq_maxee REAL          maximum expected error value for FASTQ filter"                     
# [102] "  --fastq_maxee_rate REAL     maximum expected error rate for FASTQ filter"                      
# [103] "  --fastq_maxns INT           maximum number of N's for FASTQ filter"                            
# [104] "  --fastq_minlen INT          minimum length for FASTQ filter"                                   
# [105] "  --fastq_stripleft INT       bases on the left to delete for FASTQ filter"                      
# [106] "  --fastq_trunclen INT        read length for FASTQ filter truncation"                           
# [107] "  --fastq_truncqual INT       base quality value for FASTQ filter truncation"                    
# [108] "  --relabel STRING            relabel filtered sequences with given prefix"                      
# [109] "  --relabel_keep              keep the old label after the new when relabelling"                 
# [110] "  --relabel_md5               relabel filtered sequences with md5 digest"                        
# [111] "  --relabel_sha1              relabel filtered sequences with sha1 digest"                       
# [112] "  --sizeout                   include abundance information when relabelling"                    
# [113] "  --xsize                     strip abundance information in output"                             

# [115] "FASTQ format conversion"                                                                         
# [116] "  --fastq_convert FILENAME    convert between FASTQ file formats"                                
# [117] "Options"                                                                                         
# [118] "  --fastq_ascii INT           FASTQ input quality score ASCII base char (33)"                    
# [119] "  --fastq_asciiout INT        FASTQ output quality score ASCII base char (33)"                   
# [120] "  --fastq_qmax INT            maximum base quality value for FASTQ input (41)"                   
# [121] "  --fastq_qmaxout INT         maximum base quality value for FASTQ output (41)"                  
# [122] "  --fastq_qmin INT            minimum base quality value for FASTQ input (0)"                    
# [123] "  --fastq_qminout INT         minimum base quality value for FASTQ output (0)"                   
# [124] ""                                                                                                
# [125] "FASTQ format detection and quality analysis"                                                     
# [126] "  --fastq_chars FILENAME      analyse FASTQ file for version and quality range"                  
# [127] "Options"                                                                                         
# [128] "  --fastq_tail INT            min length of tails to count for fastq_chars (4)"                  

# [130] "FASTQ paired-end reads merging"                                                                  
# [131] "  --fastq_mergepairs FILENAME merge paired-end reads into one sequence"                          
# [132] "Options:"                                                                                        
# [133] "  --eetabbedout FILENAME      output error statistics to specified file"                         
# [134] "  --fastaout FILENAME         FASTA output filename for merged sequences"                        
# [135] "  --fastaout_notmerged_fwd FN FASTA filename for non-merged forward sequences"                   
# [136] "  --fastaout_notmerged_rev FN FASTA filename for non-merged reverse sequences"                   
# [137] "  --fastq_allowmergestagger   Allow merging of staggered reads"                                  
# [138] "  --fastq_ascii INT           FASTQ input quality score ASCII base char (33)"                    
# [139] "  --fastq_eeout               include expected errors in FASTQ output"                           
# [140] "  --fastq_maxdiffs            maximum number of different bases in overlap (5)"                  
# [141] "  --fastq_maxee REAL          maximum expected error value for merged sequence"                  
# [142] "  --fastq_maxmergelen         maximum length of entire merged sequence"                          
# [143] "  --fastq_maxns INT           maximum number of N's"                                             
# [144] "  --fastq_minlen INT          minimum input read length after truncation (1)"                    
# [145] "  --fastq_minmergelen         minimum length of entire merged sequence"                          
# [146] "  --fastq_minovlen            minimum length of overlap between reads (16)"                      
# [147] "  --fastq_nostagger           disallow merging of staggered reads (default)"                     
# [148] "  --fastq_qmax INT            maximum base quality value for FASTQ input (41)"                   
# [149] "  --fastq_qmaxout INT         maximum base quality value for FASTQ output (41)"                  
# [150] "  --fastq_qmin INT            minimum base quality value for FASTQ input (0)"                    
# [151] "  --fastq_qminout INT         minimum base quality value for FASTQ output (0)"                   
# [152] "  --fastq_truncqual INT       base quality value for truncation"                                 
# [153] "  --fastqout FILENAME         FASTQ output filename for merged sequences"                        
# [154] "  --fastqout_notmerged_fwd  F FASTQ filename for non-merged forward sequences"                   
# [155] "  --fastqout_notmerged_rev  F FASTQ filename for non-merged reverse sequences"                   
# [156] "  --label_suffix              suffix to append to label of merged sequences"                     
# [157] "  --reverse FILENAME          specify FASTQ file with reverse reads"                             

# [159] "FASTQ quality statistics"                                                                        
# [160] "  --fastq_stats FILENAME      report FASTQ file statistics"                                      
# [161] "  --fastq_eestats FILENAME    quality score and expected error statistics"                       
# [162] "Options"                                                                                         
# [163] "  --fastq_ascii INT           FASTQ input quality score ASCII base char (33)"                    
# [164] "  --fastq_qmax INT            maximum base quality value for FASTQ input (41)"                   
# [165] "  --fastq_qmin INT            minimum base quality value for FASTQ input (0)"                    

# [167] "Masking (new)"                                                                                   
# [168] "  --fastx_mask FILENAME       mask sequences in the given FASTA or FASTQ file"                   
# [169] "Options"                                                                                         
# [170] "  --fastq_ascii INT           FASTQ input quality score ASCII base char (33)"                    
# [171] "  --fastq_qmax INT            maximum base quality value for FASTQ input (41)"                   
# [172] "  --fastq_qmin INT            minimum base quality value for FASTQ input (0)"                    
# [173] "  --fastaout FILENAME         output to specified FASTA file"                                    
# [174] "  --fastqout FILENAME         output to specified FASTQ file"                                    
# [175] "  --hardmask                  mask by replacing with N instead of lower case"                    
# [176] "  --max_unmasked_pct          max unmasked % of sequences to keep (100.0)"                       
# [177] "  --min_unmasked_pct          min unmasked % of sequences to keep (0.0)"                         
# [178] "  --qmask none|dust|soft      mask seqs with dust, soft or no method (dust)"                     

# [180] "Masking (old)"                                                                                   
# [181] "  --maskfasta FILENAME        mask sequences in the given FASTA file"                            
# [182] "Options"                                                                                         
# [183] "  --hardmask                  mask by replacing with N instead of lower case"                    
# [184] "  --output FILENAME           output to specified FASTA file"                                    
# [185] "  --qmask none|dust|soft      mask seqs with dust, soft or no method (dust)"                     
# [186] ""                                                                                                
# [187] "Pairwise alignment"                                                                              
# [188] "  --allpairs_global FILENAME  perform global alignment of all sequence pairs"                    
# [189] "Options (most searching options also apply)"                                                     
# [190] "  --alnout FILENAME           filename for human-readable alignment output"                      
# [191] "  --acceptall                 output all pairwise alignments"                                    

# [193] "Reverse complementation"                                                                         
# [194] "  --fastx_revcomp FILENAME    Reverse-complement seqs in FASTA or FASTQ file"                    
# [195] "Options"                                                                                         
# [196] "  --fastaout FILENAME         FASTA output filename"                                             
# [197] "  --fastq_ascii INT           FASTQ input quality score ASCII base char (33)"                    
# [198] "  --fastq_qmax INT            maximum base quality value for FASTQ input (41)"                   
# [199] "  --fastq_qmin INT            minimum base quality value for FASTQ input (0)"                    
# [200] "  --fastqout FILENAME         FASTQ output filename"                                             
# [201] "  --label_suffix STRING       Label to append to identifier in the output"                       

# [203] "Searching"                                                                                       
# [204] "  --search_exact FILENAME     filename of queries for exact match search"                        
# [205] "  --usearch_global FILENAME   filename of queries for global alignment search"                   
# [206] "Options"                                                                                         
# [207] "  --alnout FILENAME           filename for human-readable alignment output"                      
# [208] "  --blast6out FILENAME        filename for blast-like tab-separated output"                      
# [209] "  --db FILENAME               filename for FASTA formatted database for search"                  
# [210] "  --dbmask none|dust|soft     mask db with dust, soft or no method (dust)"                       
# [211] "  --dbmatched FILENAME        FASTA file for matching database sequences"                        
# [212] "  --dbnotmatched FILENAME     FASTA file for non-matching database sequences"                    
# [213] "  --fastapairs FILENAME       FASTA file with pairs of query and target"                         
# [214] "  --fulldp                    full dynamic programming alignment (always on)"                    
# [215] "  --gapext STRING             penalties for gap extension (2I/1E)"                               
# [216] "  --gapopen STRING            penalties for gap opening (20I/2E)"                                
# [217] "  --hardmask                  mask by replacing with N instead of lower case"                    
# [218] "  --id REAL                   reject if identity lower"                                          
# [219] "  --iddef INT                 id definition, 0-4=CD-HIT,all,int,MBL,BLAST (2)"                   
# [220] "  --idprefix INT              reject if first n nucleotides do not match"                        
# [221] "  --idsuffix INT              reject if last n nucleotides do not match"                         
# [222] "  --leftjust                  reject if terminal gaps at alignment left end"                     
# [223] "  --match INT                 score for match (2)"                                               
# [224] "  --matched FILENAME          FASTA file for matching query sequences"                           
# [225] "  --maxaccepts INT            number of hits to accept and show per strand (1)"                  
# [226] "  --maxdiffs INT              reject if more substitutions or indels"                            
# [227] "  --maxgaps INT               reject if more indels"                                             
# [228] "  --maxhits INT               maximum number of hits to show (unlimited)"                        
# [229] "  --maxid REAL                reject if identity higher"                                         
# [230] "  --maxqsize INT              reject if query abundance larger"                                  
# [231] "  --maxqt REAL                reject if query/target length ratio higher"                        
# [232] "  --maxrejects INT            number of non-matching hits to consider (32)"                      
# [233] "  --maxsizeratio REAL         reject if query/target abundance ratio higher"                     
# [234] "  --maxsl REAL                reject if shorter/longer length ratio higher"                      
# [235] "  --maxsubs INT               reject if more substitutions"                                      
# [236] "  --mid REAL                  reject if percent identity lower, ignoring gaps"                   
# [237] "  --mincols INT               reject if alignment length shorter"                                
# [238] "  --minqt REAL                reject if query/target length ratio lower"                         
# [239] "  --minsizeratio REAL         reject if query/target abundance ratio lower"                      
# [240] "  --minsl REAL                reject if shorter/longer length ratio lower"                       
# [241] "  --mintsize INT              reject if target abundance lower"                                  
# [242] "  --minwordmatches INT        minimum number of word matches required (10)"                      
# [243] "  --mismatch INT              score for mismatch (-4)"                                           
# [244] "  --notmatched FILENAME       FASTA file for non-matching query sequences"                       
# [245] "  --output_no_hits            output non-matching queries to output files"                       
# [246] "  --pattern STRING            option is ignored"                                                 
# [247] "  --qmask none|dust|soft      mask query with dust, soft or no method (dust)"                    
# [248] "  --query_cov REAL            reject if fraction of query seq. aligned lower"                    
# [249] "  --rightjust                 reject if terminal gaps at alignment right end"                    
# [250] "  --rowlen INT                width of alignment lines in alnout output (64)"                    
# [251] "  --samheader                 include a header in the SAM output file"                           
# [252] "  --samout FILENAME           filename for SAM format output"                                    
# [253] "  --self                      reject if labels identical"                                        
# [254] "  --selfid                    reject if sequences identical"                                     
# [255] "  --sizeout                   write abundance annotation to dbmatched file"                      
# [256] "  --slots INT                 option is ignored"                                                 
# [257] "  --strand plus|both          search plus or both strands (plus)"                                
# [258] "  --target_cov REAL           reject if fraction of target seq. aligned lower"                   
# [259] "  --top_hits_only             output only hits with identity equal to the best"                  
# [260] "  --uc FILENAME               filename for UCLUST-like output"                                   
# [261] "  --uc_allhits                show all, not just top hit with uc output"                         
# [262] "  --userfields STRING         fields to output in userout file"                                  
# [263] "  --userout FILENAME          filename for user-defined tab-separated output"                    
# [264] "  --weak_id REAL              include aligned hits with >= id; continue search"                  
# [265] "  --wordlength INT            length of words for database index 3-15 (8)"                       

# [267] "Shuffling and sorting"                                                                           
# [268] "  --shuffle FILENAME          shuffle order of sequences in FASTA file randomly"                 
# [269] "  --sortbylength FILENAME     sort sequences by length in given FASTA file"                      
# [270] "  --sortbysize FILENAME       abundance sort sequences in given FASTA file"                      
# [271] "Options"                                                                                         
# [272] "  --maxsize INT               maximum abundance for sortbysize"                                  
# [273] "  --minsize INT               minimum abundance for sortbysize"                                  
# [274] "  --output FILENAME           output to specified FASTA file"                                    
# [275] "  --randseed INT              seed for PRNG, zero to use random data source (0)"                 
# [276] "  --relabel STRING            relabel sequences with this prefix string"                         
# [277] "  --relabel_keep              keep the old label after the new when relabelling"                 
# [278] "  --relabel_md5               relabel with md5 digest of normalized sequence"                    
# [279] "  --relabel_sha1              relabel with sha1 digest of normalized sequence"                   
# [280] "  --sizeout                   include abundance information when relabelling"                    
# [281] "  --topn INT                  output just first n sequences"                                     
# [282] "  --xsize                     strip abundance information in output"                             

# [284] "Subsampling"                                                                                     
# [285] "  --fastx_subsample FILENAME  subsample sequences from given FASTA/FASTQ file"                   
# [286] "Options"                                                                                         
# [287] "  --fastaout FILENAME         output FASTA file for subsamples"                                  
# [288] "  --fastq_ascii INT           FASTQ input quality score ASCII base char (33)"                    
# [289] "  --fastq_qmax INT            maximum base quality value for FASTQ input (41)"                   
# [290] "  --fastq_qmin INT            minimum base quality value for FASTQ input (0)"                    
# [291] "  --fastqout FILENAME         output FASTQ file for subsamples"                                  
# [292] "  --randseed INT              seed for PRNG, zero to use random data source (0)"                 
# [293] "  --relabel STRING            relabel sequences with this prefix string"                         
# [294] "  --relabel_keep              keep the old label after the new when relabelling"                 
# [295] "  --relabel_md5               relabel with md5 digest of normalized sequence"                    
# [296] "  --relabel_sha1              relabel with sha1 digest of normalized sequence"                   
# [297] "  --sample_pct REAL           sampling percentage between 0.0 and 100.0"                         
# [298] "  --sample_size INT           sampling size"                                                     
# [299] "  --sizein                    consider abundance info from input, do not ignore"                 
# [300] "  --sizeout                   update abundance information in output"                            
# [301] "  --xsize                     strip abundance information in output"       

