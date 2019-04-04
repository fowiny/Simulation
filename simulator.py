from Queue import PriorityQueue
from Queue import Queue
import numpy as np
import time


class DeviceGraph(object):
    """ Describe relationship between nodes (e.g. which node can comunicate whith which) """
    def __init__(self, devices, radius=0.25):
        self.__adjacency_list = []
        self.__devices = devices
        for d1 in devices:
            self.__adjacency_list.append([])
            for d2 in devices:
                p1 = d1.point
                p2 = d2.point
                if (p1[0] - p2[0]) ** 2 + (p1[1] - p2[1]) ** 2 <= radius ** 2 and p1 != p2:
                    self.__adjacency_list[d1.index].append(d2.index)

    def connected_components(self):
        self.__cc = [-1] * len(self.__adjacency_list)
        self.__current_cc = 0
        for v in range(len(self.__adjacency_list)):
            self.__dfs(v)

    def __dfs(self, v):
        if self.__cc[v] == -1:
            self.__cc [v] = self.__current_cc
        else:
            return

        for v_next in self.__adjacency_list[v]:
            self.__dfs(v_next)

    def __len__(self):
        return len(self.__adjacency_list)

    def get_adjacent_devices(self, device):
        return [self.__devices[v] for v in self.__adjacency_list[device.index]]

    def get_devices(self):
        return self.__devices


class BroadcastPacket(object):
    """ Packet that node generate """
    __PACKET_NUMBER = 0

    def __init__(self, size):
        self.__size = size
        type(self).__PACKET_NUMBER += 1
        self.__packet_number = type(self).__PACKET_NUMBER

    @property
    def size(self):
        return self.__size

    @property
    def packet_number(self):
        return self.__packet_number


class UnicastPacket(object):
    """ Packet that node send, one broadcast is several unicast """
    def __init__(self, broadcast_packet):
        self.__packet_number = broadcast_packet.packet_number
        self.__corrupted = False

    def corrupt(self):        
        self.__corrupted = True

    def is_corrupted(self):
        """ Is packet corrupted due collision """
        return self.__corrupted

    @property
    def packet_number(self):
        return self.__packet_number


class Device(object):
    """ Describe node state, messages in buffer, sending and receiving messages """
    def __init__(self, index, point, buffer_size):
        self.__index = index
        self.__buffered_packets = Queue()
        self.__sending_packet = None
        self.__receiving_packets = {}
        self.__point = point
        self.__buffer_size = buffer_size

    @property
    def point(self):
        return self.__point

    @property
    def index(self):
        return self.__index

    def get_sending_packet(self):
        return self.__sending_packet

    def channel_clear(self):
        return self.__sending_packet is None and len(self.__receiving_packets) == 0

    def buffer_full(self):
        return self.__buffered_packets.qsize() == self.__buffer_size

    def buffer_empty(self):
        return self.__buffered_packets.empty()

    def packets_in_buffer(self):
        return self.__buffered_packets.qsize()

    def get_packet_from_buffer(self):
        return self.__buffered_packets.get()

    def buffer_packet(self, packet):
        self.__buffered_packets.put(packet)

    def send_packet(self, packet):
        self.__sending_packet = packet

    def receive_packet(self, packet):
        for receiving_packet in self.__receiving_packets.values():
            receiving_packet.corrupt()
        if len(self.__receiving_packets) != 0:
            packet.corrupt()
        self.__receiving_packets[packet.packet_number] = packet

    def send_done(self):
        self.__sending_packet = None

    def receive_done(self, packet):
        return self.__receiving_packets.pop(packet.packet_number)


class Simulation(object):
    """ Simulation of network using DES """
    def __init__(self, packet_arrival_distribution, packet_length_distribution, device_graph,
                 link_speed = 8 * 1024 * 1024, simulation_time=60 * 60, queue_level_sampling_time=0.0):
        self.__packet_arrival_distribution = packet_arrival_distribution
        self.__packet_length_distribution = packet_length_distribution
        self.__device_graph = device_graph
        self.__link_speed = float(link_speed)
        self.__event_queue = PriorityQueue()
        self.__devices = device_graph.get_devices()
        self.__simulation_time = simulation_time
        self.__queue_level_sampling_time = queue_level_sampling_time

        self.__number_of_lost_packets = [0] * len(self.__devices)
        self.__number_of_broadcast_packets = [0] * len(self.__devices)
        self.__number_of_sent_to_unicast_packets = [0] * len(self.__devices)
        self.__number_of_received_at_unicast_packets = [0] * len(self.__devices)

        self.__size_of_lost_packets = [0] * len(self.__devices)
        self.__size_of_broadcast_packets = [0] * len(self.__devices)
        self.__size_of_sent_to_unicast_packets = [0] * len(self.__devices)
        self.__size_of_received_at_unicast_packets = [0] * len(self.__devices)


        self.__queue_levels = [[] for i in range(len(self.__devices))]

    def __send_packet(self, timestamp, device, broadcast_packet):
        device.send_packet(broadcast_packet)
        for receiver in self.__device_graph.get_adjacent_devices(device):
            unicast_packet = UnicastPacket(broadcast_packet)
            receiver.receive_packet(unicast_packet)
        self.__event_queue.put((timestamp + broadcast_packet.size/self.__link_speed, device.index,
                                type(self).__packet_sent))

    def __buffer_packet(self, timestamp, device, broadcast_packet):
        if device.buffer_full():
            self.__number_of_lost_packets[device.index] += 1
            self.__size_of_lost_packets[device.index] += broadcast_packet.size
        else:
            device.buffer_packet(broadcast_packet)

    def __packet_arrived(self, timestamp, device):
        packet_size = self.__packet_length_distribution()
        broadcast_packet = BroadcastPacket(packet_size)
        self.__number_of_broadcast_packets[device.index] += 1
        self.__size_of_broadcast_packets[device.index] += broadcast_packet.size
        if device.channel_clear():
            self.__send_packet(timestamp, device, broadcast_packet)
        else:
            self.__buffer_packet(timestamp, device, broadcast_packet)
        next_packet_in = self.__packet_arrival_distribution()
        self.__event_queue.put((timestamp + next_packet_in, device.index, type(self).__packet_arrived))

    def __packet_sent(self, timestamp, device):
        packet = device.get_sending_packet()
        device.send_done()
        adjacent_devices = self.__device_graph.get_adjacent_devices(device)
        for d in adjacent_devices:
            received_packet = d.receive_done(packet)
            self.__number_of_sent_to_unicast_packets[d.index] += 1
            self.__size_of_sent_to_unicast_packets[d.index] += packet.size
            if not received_packet.is_corrupted():
                self.__number_of_received_at_unicast_packets[d.index] += 1
                self.__size_of_received_at_unicast_packets[d.index] += packet.size

        devices_that_can_send = [device] + adjacent_devices
        for d in devices_that_can_send:
            if d.channel_clear() and not d.buffer_empty():
                next_packet = d.get_packet_from_buffer()
                self.__send_packet(timestamp, d, next_packet)

    def __sample_queue_level(self, timestamp, device):
        self.__queue_levels[device.index].append( (timestamp, device.packets_in_buffer()) )
        self.__event_queue.put((timestamp + self.__queue_level_sampling_time, device.index,
                 type(self).__sample_queue_level))

    def run(self):
        current_time = 0
        for device in self.__devices:
            next_packet_in = self.__packet_arrival_distribution()
            self.__event_queue.put((current_time + next_packet_in, device.index,
                                    type(self).__packet_arrived))
            if self.__queue_level_sampling_time != 0.0:
                self.__event_queue.put((current_time + self.__queue_level_sampling_time, device.index,
                     type(self).__sample_queue_level))

        while current_time < self.__simulation_time:
            event = self.__event_queue.get()
            current_time = event[0]
            device = self.__devices[event[1]]
            event[2](self, current_time, device)

    def get_throughput_packets(self):
        return [float(number) / self.__simulation_time for number in
                self.__number_of_received_at_unicast_packets]

    def get_throughput_bits(self):
        return [8.0 * float(size) / self.__simulation_time for size in
                self.__size_of_received_at_unicast_packets]

    def get_loss_rate(self):
        return [float(lost)/float(sent) for lost, sent
                in zip(self.__number_of_lost_packets, self.__number_of_broadcast_packets)]

    def get_collision_rate(self):
        return [float(sent - received)/float(sent) if sent != 0 else 1 for received, sent
                in zip(self.__number_of_received_at_unicast_packets,
                       self.__number_of_sent_to_unicast_packets)]

    def get_queue_levels(self):
        return self.__queue_levels


def run_one_simulation(arrival_param, seed, simulation_time, queue_level_sampling_time=0.0):
    points = [(0.549, 0.77),(0.268, 0.732),(0.516, 0.555),(0.308, 0.268),(0.66, 0.57),
              (0.474, 0.623),(0.751, 0.697),(0.383, 0.133),(0.468, 0.456),(0.192, 0.558)]
    devices = [Device(index, point, 32) for index, point in enumerate(points)]
    device_graph = DeviceGraph(devices, 0.25)
    np.random.seed(seed)
    simulation = Simulation(lambda: np.random.uniform(0, arrival_param),
                            lambda: np.random.uniform(32, 4733), device_graph,
                            simulation_time=simulation_time,
                            queue_level_sampling_time = queue_level_sampling_time)
    simulation.run()
    return simulation


def run_all_simulations(file_name="simulation_result.csv"):
    """ Run all simulation to produce CI """
    arrival_params =  1.5 / np.linspace(1, 90, num = 10)
    seed_space = range(50)
    f = open(file_name, 'w')
    print >>f, "param, seed, node, throughput_bits, throughput_packets, loss_rate, collision_rate"
    for n, arrival_param in enumerate(arrival_params):
        for seed in seed_space:
            simulation = run_one_simulation(arrival_param, seed, 60)
            throughput_packets = simulation.get_throughput_packets()
            throughput_bits = simulation.get_throughput_bits()
            loss_rate = simulation.get_loss_rate()
            collision_rate = simulation.get_collision_rate()
            for node in range(10):
                print >>f, "%s, %s, %s, %s, %s, %s, %s" % (arrival_param, seed, node, throughput_bits[node],
                                            throughput_packets[node], loss_rate[node], collision_rate[node])
            f.flush()
            print ("Param number: %s, seed: %s; completed: %s" %
                (n, seed, (n * len(seed_space) + seed + 1) / float(len(arrival_params) * len(seed_space))))
    f.close()


def measure_queue_levels(file_name="queue_levels.csv"):
    """ Measure queue levels for queue_level plot """
    param = 0.0175
    seed = 0
    f = open(file_name, 'w')
    print >>f, "time, node, queue_level"
    simulation = run_one_simulation(param, seed, 60, queue_level_sampling_time=0.05)
    levels = simulation.get_queue_levels()
    for node in range(len(levels)):
        for time, level in levels[node]:
            print >>f, "%s, %s, %s" % (time, node, level)

    f.close()


if __name__ == "__main__":
    print "Measuring queue levels..."
    measure_queue_levels()
    print "Run multiple experiment for CI..."
    start = time.time()
    run_all_simulations()
    end = time.time()
    print(end - start)