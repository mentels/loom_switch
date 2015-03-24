log_file = 'log/notice.log'
one_file = 'log/packet_in_one.metrics'
count_file = 'log/packet_in_count.metrics'

with open(log_file) as f, open(one_file, 'w') as one, open(count_file, 'w') as count:
    for line in f:
        if 'packet_in_one' in line:
            splitted = line.split()
            time = splitted[1]
            value = splitted[5].split(":")[1]
            one.write('%s %s\n' % (time, value))
        elif 'packet_in_count' in line:
            splitted = line.split()
            time = splitted[1]
            value = splitted[5].split(":")[1]
            count.write('%s %s\n' % (time, value))

