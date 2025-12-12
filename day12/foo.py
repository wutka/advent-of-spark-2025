import re

region_re = re.compile ("([0-9][0-9])x([0-9][0-9]): (.*)")

infile = open("data/day12.txt")
line_num = 0
cap_count = 0
nocap_count = 0
for line in infile:
    line = line.strip()
    line_num = line_num + 1
    match = re.match(region_re, line)

    if match is not None:
        width = int(match.group(1))
        height = int(match.group(2))
        nums = [int(x) for x in match.group(3).split(" ")]
        sum = 0
        for n in nums:
            sum = sum + n * 7
        if sum <= width * height:
            cap_count += 1
            print("free space at {} = {}".format(line_num, width * height - sum))
        else:
            nocap_count += 1
print("{} have capacity, {} do not".format(cap_count, nocap_count))
