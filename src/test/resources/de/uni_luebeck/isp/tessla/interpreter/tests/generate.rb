# Script to generate input and output files from CSV files with the following format:
# * Header row (first line) contains the variable names with prefix `in` for input and
#   `out` for output.
# * Columns without matching prefix will be ignored.
# * First column must contain the timestamp of the row.
# 
# For example `foo.csv`
# ```
# ts;in x;out y
# 1;2;3
# 4;5;6
# ```
# will be converted to `foo.input`
# ```
# 1: x = 2
# 4: x = 5
# ```
# and `foo.output`
# ```
# 1: y = 3
# 4: y = 6
# ```

header, data = File.open(ARGV[0]) do |f|
  [f.readline.strip.split(/[,;]/), f.read.split("\n").map{|l| l.strip.split(/[,;]/)}]
end

def blank?(x)
  x.nil? || x == ""
end

def gen(ts, var, value)
  "#{ts}: #{var} = #{value}\n"
end

def create(key, header, data)
  data.map do |line|
    line.zip(header)[1..-1].select { |el, hd| hd.start_with?(key) && !blank?(el) }.map do |el, hd|
      gen(line[0], hd[key.length+1..-1], el)
    end
  end.flatten
end

def write(filename, key, header, data)
  events = create(key, header, data)
  File.write(filename, events.join)
  puts "Written #{filename} with #{events.count} events."
end

basename = ARGV[0].sub(/\.[^.]+$/, '')
write(basename + ".output", "out", header, data)
write(basename + ".input", "in", header, data)
