Saurabh Shirodkar and Pratheek Mallya

DS4CG MAPC

This file describes the process of creating targets and executing the algorithm for the MAPC reweighting procedure.

The target tables should be entered into the file "reweighting_config.json". The format of the file can be understood from the following example:

```

{
    "file_name": "testhh.csv",
    "blocks" : [
        {
            "target_var":  "PWGTP",
            "special_cond_var": "none",
            "special_cond_target": "none",
            "tables": [
                {
                  "name": "FirstTable",
                  "dims": [
                    {
                      "var": "AGEP",
                      "type": "NUM",
                      "conditions": [
                        "x <= 2 ",
                        "2 < x & x <= 4",
                        "4 < x & x <= 6",
                        "x > 6"
                      ]
                    },
                    {
                        "var": "SEX",
                        "type": "NUM",
                        "conditions": [
                            "x == 1",
                            "x == 2"
                        ]
                    },
                    {
			                "var": "PLACE",
			                "type": "NUM",
			                "conditions": [
			                  "x==1",
			                  "x==2"
			                ]
		                }
                  ]
                }
            ]
        },
        {
            "target_var": "WGTP",
            "special_cond_var": "SPORDER",
            "special_cond_target": "1",
            "tables": [
                {
                    "name": "SecondTable",
                    "dims": [
                        {
                            "var": "ADJINC",
                            "type": "NUM",
                            "conditions": [
                                "x == 3",
                                "x == 4",
                                "x == 5",
                                "x == 6"
                            ]
                        },
                        {
                            "var": "HHTYPE",
                            "type": "NUM",
                            "conditions": [
                                "x == 1",
                                "x == 2",
                                "x > 3"
                            ]
                        },
                        {
			                    "var": "PLACE",
			                    "type": "NUM",
			                    "conditions": [
			                      "x==1",
			                      "x==2"
			                    ]
		                    }
                    ]
                }
            ]
        }
    ]
}

```

> * "file_name" specifies the input CSV file (the PUMS dataset).

> * Each block represents a different target variable, along with any special conditions to be imposed on the block.

> * "target_var" specifies the name of the target variable for the tables in the particular block.

> * "special_cond_var" specifies the name of the variable under which any special filtering condition needs to be applied.

> * "special_cond_target" specifies the value of the special condition variable for filtering. 

Each block consists of a set of tables.

Each object in the "tables" array represents a table. There can be an arbitrary amount of tables.

For each table, the "name" field indicates the name of the table. Each object of the "dims" array indicates one variable or dimension. The "var" field of a variable should be a header in the input CSV file. The "type" field can take values "NUM" or "CHAR" and denotes the type of the variable. The "conditions" array is a set of conditions that represent the columns of the table corresponding to the varaible. Each of these should be a logical expression (that evaluates to a boolean) that can be parsed by the R interpreter. The variable in this expression should be denoted by the letter 'x'.

The PLACE variable denotes the geography of the household. The code only works properly when the PLACE variable is mentioned last in the table in the config file.  This is crucial since we update the ids of the data records holding this assumption.

The number of dimensions, as well as the number of conditions within each dimension, can be arbitrarily large. 

After specifying "split_config.json" in this format, run the script "read_config_hh.R". Corresponding to each table, a CSV file will be generated with the same name as that table. Open the CSV file for each table (using a software such as MS Excel or LibreOffice Calc, or a text editor) and fill in the target values corresponding to each cell of the target table.

After specifying the target tables, run the script "test.R". The number of iterations can be modified at the top of the script. At the end of the script, the output weights will be written to the file "split_final.csv".

Dependencies:
R Packages:

* tidyverse
* rjson
* data.table
