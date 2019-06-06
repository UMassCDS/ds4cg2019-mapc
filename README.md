Saurabh Shirodkar and Pratheek Mallya

DS4CG MAPC

This file describes the process of creating targets for the MAPC reweighting procedure.

The target tables should be entered into the file "reweighting_config.json". The format of the file can be understood from the following example:

{
  "tables": [
    {
      "name": "FirstTable",
      "dims": [
        {
          "var": "AGE",
          "type": "NUM",
          "conditions": [
            "-Inf < x & x < 20",
            "21 < x & x < 50",
            "51 < x & x < +Inf"
          ]
        },
        {
          "var": "INCOME",
          "type": "NUM",
          "conditions": [
            "x <= 50000",
            "x > 50000"
          ]
        },
        {
          "var": "GENDER",
          "type": "NUM",
          "conditions": [
            "x == 0",
            "x == 1"
          ]
        }
      ]
    },
    {
      "name": "SecondTable",
      "dims": [
        {
          "var": "EDUCATION",
          "type": "NUM",
          "conditions": [
            "x == 1",
            "x == 2",
            "x == 3"
          ]
        }
      ]
    }
  ]
}

Each object in the "tables" array represents a table. There can be an arbitrary amount of tables.

For each table, the "name" field indicates the name of the table. Each object of the "dims" array indicates one variable or dimension. The "var" field of a variable should be a header in the input CSV file(the PUMS dataset). The "type" field can take values "NUM" or "CHAR" and denotes the type of the variable. The "conditions" array is a set of conditions that represent the columns of the table corresponding to the varaible. Each of these should be a logical expression(that evaluates to a boolean) that can be parsed by the R interpreter. The variable in this expression should be denoted by the letter 'x'.

The number of dimensions, as well as the number of conditions within each dimension, can be arbitrarily large. In the above example, the dimension of "FirstTable" is 3x2x2 while that of "SecondTable" is just 3.

After specifying "reweighting_config.json" in this format, run the script "read_config.R". Corresponding to each table, a CSV file will be generated with the same name as that table. Open the CSV file for each table(using a software such as MS Excel) and fill in the target values corresponding to each cell of the target table.

Dependencies:
rjson (R package)