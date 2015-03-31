log_file = 'log/notice.log'
count_file = 'log/packet_in.metrics'

with open(log_file) as f, open(count_file, 'w') as count:
    prev_count = 0
    for line in f:
        if 'packet_in_count' in line:
            splitted = line.split()
            time = splitted[1].split('.')[0]
            count_value = int(splitted[5].split(":")[1])
            calc_one_value = count_value - prev_count
            prev_count = count_value
            count.write('%s %d %d \n' % (time, count_value, calc_one_value))
