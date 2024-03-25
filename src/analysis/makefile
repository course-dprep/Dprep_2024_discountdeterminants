DATA = ../../gen/data
AUDIT = ../../gen/audit
OUTPUT_1 = ../gen/firststage
OUTPUT_2 = ../gen/secondstage
REPORT = ../../gen

all: $(OUTPUT_1)/df_firststage.csv $(OUTPUT_1)/plot_mean_elasticity.png $(OUTPUT_2)/secondstage_ls.rds $(REPORT)/report.pdf

$(OUTPUT_1)/df_firststage.csv: $(DATA)/df_aggregate.csv $(DATA)/categorycharacteristic.csv $(DATA)/brandcharacteristic.csv firststage.R 
	R --vanilla < firststage.R

$(OUTPUT_1)/plot_mean_elasticity.png: $(DATA)/df_aggregate.csv $(DATA)/categorycharacteristic.csv $(DATA)/brandcharacteristic.csv firststage.R 
	R --vanilla < firststage.R

$(OUTPUT_2)/secondstage_ls.rds: $(OUTPUT_1)/df_firststage.csv secondstage.R 
	R --vanilla < secondstage.R

$(REPORT)/report.pdf: $(AUDIT)/discounts_sales_brand_a.png $(AUDIT)/discounts_sales_brand_b.png $(AUDIT)/discounts_sales_brand_c.png $(AUDIT)/discounts_sales_brand_d.png $(AUDIT)/discounts_sales_brand_e.png $(AUDIT)/discounts_sales_in_hypermarket.png $(AUDIT)/discounts_sales_in_supermarket.png $(AUDIT)/discounts_sales_in_convenience.png $(OUTPUT_1)/plot_mean_elasticity.png $(OUTPUT_2)/secondstage_ls.rds report.Rmd gen_report.R
	R --vanilla < gen_report.R