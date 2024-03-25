PREP = ../data
OUTPUT = ../gen/data
AUDIT = ../gen/audit

all: $(OUTPUT)/df_aggregate.csv $(OUTPUT)/categorycharacteristic.csv $(OUTPUT)/brandcharacteristic.csv $(AUDIT)/summary_stat.csv $(AUDIT)/discounts_sales_brand_a.png $(AUDIT)/discounts_sales_brand_b.png $(AUDIT)/discounts_sales_brand_c.png $(AUDIT)/discounts_sales_brand_d.png $(AUDIT)/discounts_sales_brand_e.png $(AUDIT)/discounts_sales_in_hypermarket.png $(AUDIT)/discounts_sales_in_supermarket.png $(AUDIT)/discounts_sales_in_convenience.png

$(OUTPUT)/df_aggregate.csv: $(PREP)/df.csv data-preparation.R 
	R --vanilla < data-preparation.R

$(OUTPUT)/categorycharacteristic.csv: $(PREP)/df.csv data-preparation.R 
	R --vanilla < data-preparation.R

$(OUTPUT)/brandcharacteristic.csv: $(PREP)/df.csv data-preparation.R 
	R --vanilla < data-preparation.R

$(AUDIT)/summary_stat.csv: $(PREP)/df.csv $(OUTPUT)/df_aggregate.csv $(OUTPUT)/categorycharacteristic.csv $(OUTPUT)/brandcharacteristic.csv audit.R 
	R --vanilla < audit.R

$(AUDIT)/discounts_sales_brand_a.png: $(PREP)/df.csv $(OUTPUT)/df_aggregate.csv $(OUTPUT)/categorycharacteristic.csv $(OUTPUT)/brandcharacteristic.csv audit.R 
	R --vanilla < audit.R

$(AUDIT)/discounts_sales_brand_b.png: $(PREP)/df.csv $(OUTPUT)/df_aggregate.csv $(OUTPUT)/categorycharacteristic.csv $(OUTPUT)/brandcharacteristic.csv audit.R 
	R --vanilla < audit.R

$(AUDIT)/discounts_sales_brand_c.png: $(PREP)/df.csv $(OUTPUT)/df_aggregate.csv $(OUTPUT)/categorycharacteristic.csv $(OUTPUT)/brandcharacteristic.csv audit.R 
	R --vanilla < audit.R

$(AUDIT)/discounts_sales_brand_d.png: $(PREP)/df.csv $(OUTPUT)/df_aggregate.csv $(OUTPUT)/categorycharacteristic.csv $(OUTPUT)/brandcharacteristic.csv audit.R 
	R --vanilla < audit.R

$(AUDIT)/discounts_sales_brand_e.png: $(PREP)/df.csv $(OUTPUT)/df_aggregate.csv $(OUTPUT)/categorycharacteristic.csv $(OUTPUT)/brandcharacteristic.csv audit.R 
	R --vanilla < audit.R

$(AUDIT)/discounts_sales_in_hypermarket.png: $(PREP)/df.csv $(OUTPUT)/df_aggregate.csv $(OUTPUT)/categorycharacteristic.csv $(OUTPUT)/brandcharacteristic.csv audit.R 
	R --vanilla < audit.R

$(AUDIT)/discounts_sales_in_supermarket.png: $(PREP)/df.csv $(OUTPUT)/df_aggregate.csv $(OUTPUT)/categorycharacteristic.csv $(OUTPUT)/brandcharacteristic.csv audit.R 
	R --vanilla < audit.R

$(AUDIT)/discounts_sales_in_convenience.png: $(PREP)/df.csv $(OUTPUT)/df_aggregate.csv $(OUTPUT)/categorycharacteristic.csv $(OUTPUT)/brandcharacteristic.csv audit.R 
	R --vanilla < audit.R

	