Saurabh Shirodkar and Pratheek Mallya

DS4CG MAPC

This file describes the process of creating targets and executing the algorithm for the MAPC reweighting procedure.

The target tables should be entered into the file "reweighting_config.json". The format of the file can be understood from the following example:

```

{
  "file_name": "ss16pma.csv",
  "target_var":  "PWGTP",
  "tables": [
    {
      "name": "FirstTable",
      "dims": [
        {
          "var": "AGEP",
          "type": "NUM",
          "conditions": [
            "x <= 20",
            "20 < x & x <= 40",
            "40 < x & x <= 60",
            "x > 60"
          ]
        },
        {
          "var": "WAGP",
          "type": "NUM",
          "conditions": [
            "x <= 50000",
            "x > 50000 & x <= 80000",
            "x > 80000 & x <= 120000",
            "x > 120000"
          ]
        }
      ]
    },
    {
      "name": "SecondTable",
      "dims": [
        {
          "var": "SEX",
          "type": "NUM",
          "conditions": [
            "x == 1",
            "x == 2"
          ]
        }
      ]
    }
  ]
}

```

"file_name" specifies the input CSV file(the PUMS dataset).

"target_var" specifies the name of the target variable in the input CSV file.

Each object in the "tables" array represents a table. There can be an arbitrary amount of tables.

For each table, the "name" field indicates the name of the table. Each object of the "dims" array indicates one variable or dimension. The "var" field of a variable should be a header in the input CSV file. The "type" field can take values "NUM" or "CHAR" and denotes the type of the variable. The "conditions" array is a set of conditions that represent the columns of the table corresponding to the varaible. Each of these should be a logical expression(that evaluates to a boolean) that can be parsed by the R interpreter. The variable in this expression should be denoted by the letter 'x'.

The number of dimensions, as well as the number of conditions within each dimension, can be arbitrarily large. In the above example, the dimension of "FirstTable" is 3x2x2 while that of "SecondTable" is just 3.

After specifying "reweighting_config.json" in this format, run the script "read_config.R". Corresponding to each table, a CSV file will be generated with the same name as that table. Open the CSV file for each table(using a software such as MS Excel) and fill in the target values corresponding to each cell of the target table.

After specifying the target tables, run the script "test.R". The number of iterations can be modified at the top of the script. At the end of the script, the output weights will be written to the file "weights_final.csv".

Dependencies:
R Packages:

* tidyverse
* rjson
* data.table
