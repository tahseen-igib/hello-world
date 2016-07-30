#### R programme for vcf to genotype conversion ###########
rm(list=ls())
vadu.vcf.file<-"/home/tahseen_abbas/Documents/vadu_data/post-vcf/filtered_snps/allele_freq/all_snp_data/allndfiltered_snps/allele_freq_V_P_K_CONTROL/SNPs.recode.vcf"

vadu.vcf<-read.table(vadu.vcf.file,
			header=FALSE,
			stringsAsFactors=FALSE,
			skip=125,
			sep='\t')
save(vadu.vcf, 
	file="./vadu.vcf.Rdata")
vcf.header <- readLines(vadu.vcf.file,
			  n=126)	
vcf.header <- vcf.header[126]
vcf.header <- gsub("#", "", vcf.header)	
------------ extracting 107snps from vadu.vcf -------------------------
snps<-read.table('107snps.csv',header=FALSE,stringsAsFactors=FALSE)
 snps.only<-as.vector(snps[1,])
 snps.only.index<-which(chr.pos %in% snps.only)
 snps.only.vadu<-vadu.vcf[snps.only.index,]
multi_alt<-snps.only.vadu[54,]
snps.only.vadu<-snps.only.vadu[-54,]
dim(snps.only.vadu)
#[1] 106  72



convert<-function(x){
	x.list<-strsplit(as.character(x[,10:72]),split=':');
	allele <- unlist(lapply(x.list,function(x){x[1]}));
	split_allele<-strsplit(allele,split='/');
	ori.geno <- c(ref = "O", alt = "1")
	rep.geno <- c("0" = x[,4], "1" = x[,5])
	new.geno <- lapply(split_allele,function(x){rep.geno[x]})
	geno <-lapply(new.geno,function(x){paste(x,collapse='/')})
	return(geno);
}

apply(snps.only.vadu,1,convert)

