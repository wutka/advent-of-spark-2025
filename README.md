# Advent of Code 2025 in SPARK

You will need the Alire (https://alire.ada.dev/) to build each day's code.
You will also need to download that day's input and put it in a folder
named `data` with the name `day#.txt` where # is the day number.
For example, the code for day 3 is in day3, and in day3, you need
to make a data directory containing a `day3.txt`. Then to run the day3
code, cd to day3 and do:
```
alr run
```

You can also invoke `gnatprove` to prove the absence of runtime errors.
You must at least do `alr gnatprove --level=silver`, but many times
you also need to add `--level=2` to successfully prove some of the
math expressions. The `README.md` file for each day tells you what
arguments to need for `gnatprove`.
