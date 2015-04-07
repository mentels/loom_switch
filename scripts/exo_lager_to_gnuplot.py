# import datetime.datetime as datetime
import datetime as dt

log_file = 'log/notice.log'
pkt_in_count_file = 'pkt_in.metrics'
handle_pkt_in_mean_file = 'handle_pkt_in.metrics'


def seconds_from_start(raw_time):
    t = dt.datetime.strptime(raw_time, "%H:%M:%S")
    if hasattr(seconds_from_start, "init_datetime"):
        diff = t - seconds_from_start.init_datetime
        return diff.total_seconds()
    else:
        seconds_from_start.init_datetime = t
        return 0


with open(log_file) as f, open(pkt_in_count_file, 'w') as pkt_in:
    prev_count = 0
    for line in f:
        if 'packet_in_count' in line:
            splitted = line.split()
            time = splitted[1].split('.')[0]
            secs_from_start = seconds_from_start(time)
            count_value = int(splitted[5].split(":")[1])
            calc_one_value = count_value - prev_count
            prev_count = count_value
            pkt_in.write('%d %d %d \n' % (secs_from_start, count_value,
                                          calc_one_value))

with open(log_file) as f, open(handle_pkt_in_mean_file, 'w') as handle_pkt_in:
    for line in f:
        if 'handle_packet_in_mean' in line:
            splitted = line.split()
            time = splitted[1].split('.')[0]
            secs_from_start = seconds_from_start(time)
            value = int(splitted[5].split(":")[1])
            handle_pkt_in.write('%d %d \n' % (secs_from_start, value))
