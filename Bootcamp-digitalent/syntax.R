library(ggplot2)
pelanggan<-read.csv("https://storage.googleapis.com/dqlab-dataset/customer_segments.txt", sep="\t")

pelanggan[c('Jenis.Kelamin',"Umur","Profesi","Tipe.Residen")]

field_yang_digunakan<-pelanggan[c('Jenis.Kelamin',"Umur","Profesi")]
field_yang_digunakan

pelanggan_matrix<-data.matrix(field_yang_digunakan)
pelanggan_matrix

#Normalisasi Nilai Belanja
pelanggan<-data.frame(pelanggan,pelanggan_matrix)
pelanggan$NilaiBelanjaSetahun<-pelanggan$NilaiBelanjaSetahun/1000000
pelanggan
Profesi <- unique(pelanggan[c("Profesi","Profesi.1")])
Jenis.Kelamin <- unique(pelanggan[c("Jenis.Kelamin","Jenis.Kelamin.1")])
Tipe.Profesi <- unique(pelanggan[c("Tipe.Residen","Tipe.Residen.1")])
pelanggan$NilaiBelanjaSetahun <- pelanggan$NilaiBelanjaSetahun/1000000
field_yang_digunakan = c("Jenis.Kelamin.1", "Umur", "Profesi.1", "Tipe.Residen.1","NilaiBelanjaSetahun")
#Bagian K-Means
set.seed(100)
#fungsi kmeans untuk membentuk 5 cluster dengan 25 skenario random dan simpan ke dalam variable segmentasi
segmentasi <- kmeans(x=pelanggan[c("Umur","Profesi.1")],centers =3,nstart = 25)
#tampilkan hasil k-means
segmentasi

pelanggan$cluster<-segmentasi$cluster

str(pelanggan)

which(pelanggan$cluster==1)

pelanggan

# looping with SSE terkecil
sse<-sapply(1:10, function(param_k){
  kmeans(pelanggan_matrix,param_k,nstart=25)$tot.withinss
})

sse


jumlah_cluster_max<-10
ssdata=data.frame(cluster=c(1:jumlah_cluster_max),sse)
ssdata

ggplot(ssdata,aes(x=cluster,y=sse)) +
  geom_line(color="red") + geom_point()+
  ylab("Within Cluster Sum of Squares") + 
  xlab("Jumlah Cluster") + geom_text(aes(label=format(round(sse,2),nsmall=2)),hjust=-0.2, vjust=-0.5) +
  scale_x_discrete(limits=c(1:jumlah_cluster_max))

Segmen.Pelanggan <- data.frame(cluster=c(1,2,3,4,5), Nama.Segmen=c("Silver Youth Gals", "Diamond Senior Member", "Gold Young Professional", "Diamond Professional", "Silver Mid Professional"))


databaru<-data.frame(Customer_ID="CUST-100",Nama.Pelanggan="Rudi Wilamar",Umur=20,Jenis.Kelamin='Wanita',Profesi="Pelajar",Tipe.Residen="Cluster",NilaiBelanjaSetahun=3.5)
databaru

saveRDS(pelanggan,file="pelanggancluster.rds")

readRDS(file='pelanggancluster.rds')

databaru <- data.frame(Customer_ID="CUST-100", Nama.Pelanggan="Rudi Wilamar",Umur=32,Jenis.Kelamin="Wanita",Profesi="Pelajar",Tipe.Residen="Cluster",NilaiBelanjaSetahun=3.5)


databaru <- merge(databaru, Identitas.Cluster$Profesi)
databaru <- merge(databaru, Identitas.Cluster$Jenis.Kelamin)
databaru <- merge(databaru, Identitas.Cluster$Tipe.Residen)

#menentukan data baru di cluster mana
which.min(sapply( 1:5, function( x ) sum( ( databaru[Identitas.Cluster$field_yang_digunakan] - Identitas.Cluster$Segmentasi$centers[x,])^2 ) ))
Identitas.Cluster$Segmen.Pelanggan[which.min(sapply( 1:5, function( x ) sum( ( databaru[Identitas.Cluster$field_yang_digunakan] - Identitas.Cluster$Segmentasi$centers[x,])^2 ) )),]
