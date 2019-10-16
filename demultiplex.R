# Set working directory (to where data is located)
setwd("/Users/kevin/Desktop/CPSC 441/Project/")
getwd()

# Import barcodes from csv file
barcodes <- read.csv("barcodes.csv", head = TRUE)
head(barcodes)
males <- as.vector(barcodes[,2])
females <- as.vector(barcodes[,1])
class(females)
males

# Import reads from fastq file
reads<-read.delim("head_RAD_seq.fq", head = FALSE, sep = "\t")
head(reads)

# Test to make sure the reads vector has 100000 lines as expected
nrow(reads)

#initiate a custom fuction that will split a sequence line at the restriction site
#and return the barcode and the sequence associated with it
split.read <- function(line) {
  strsplit(as.character(line), "AGCTT")
}

# An increment function to add count to a variable (R's equivalent of +=)
increment <- function(variable, x) {
  eval.parent(substitute(variable <- variable + x))
}

# Read head for small samples for easier testing
head_reads<-as.vector(head(reads, 100))
class(reads)
head(head_reads)

# Initialize some variables to serve as counters
female_count<-0
male_count<-0
error_count<-0

# Initialize a data frame to serve as hash table to count barcodes
df.barcode <- data.frame()

# Loop through the reads vector
for (i in 1:nrow(reads)) {
  if (i%%4 == 2) {
    
    split_read<-split.read(reads[i,1])  # Split the sequence at restriction
    
    # Add in first barcode read to create first column in data frame
    if (nrow(df.barcode) == 0) { 
      df.barcode[nrow(df.barcode) + 1, 1] = split_read[[1]][1]
      df.barcode[nrow(df.barcode), 2] = 1
    }
    
    # Count occurance of each barcode by adding it to hash map (implemented by data frame)
    if (split_read[[1]][1] %in% df.barcode[,1]) { 
      increment(df.barcode[df.barcode[,1] == split_read[[1]][1],2], 1)
    } else {
      df.barcode[nrow(df.barcode) + 1, 1] = split_read[[1]][1]
      df.barcode[nrow(df.barcode), 2] = 1
    }
    
    # Count amount of males and females
    if (split_read[[1]][1] %in% males) {  
      increment(male_count, 1)
    } else if (split_read[[1]][1] %in% females) {
      increment(female_count, 1)
    } else {
      increment(error_count, 1)
    }
  }
} 

# Create another data frame and iterate through hash map to exclude error barcodes
df.newhash <- df.barcode
for (i in 1:nrow(df.newhash)) {
  if((!df.newhash[i,1] %in% males) & (!df.newhash[i,1] %in% females)) {
    df.newhash <- df.newhash[-i,]  # Delete rows with error barcodes.
  }
}

# Histogram and hash map of barcodes count (including errors)
head(df.barcode)
hist(df.barcode$V2, 
     col="tan",
     main="Barcode Counts",
     xlab="Number of Reads",
     ylab="Number of Indiviudals")

# Histogram and hash map of correct barcodes count
head(df.newhash)
hist(df.newhash$V2, 
     col="green",
     main="Barcode Counts",
     xlab="Number of Reads",
     ylab="Number of Indiviudals")

# Creates a graph on gender distribution
barplot(c(male_count, female_count, error_count), 
        main="Gender Distribution", horiz=TRUE, 
        col= c("light blue", "pink", "black"),
        names.arg=c("Male", "Female", "Error"),
        xlab="Number of Reads",
        ylab="Type of Read")
