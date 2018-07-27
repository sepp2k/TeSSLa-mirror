#!/usr/bin/env ruby

def print_usage(io)
  io.puts "Usage: #$0 [options] category/test-case-name"
  io.puts "       #$0 --help"
  io.puts ""
  io.puts "Options:"
  io.puts "  --help                Print this help message and exit"
  io.puts "  --interpreter         Create a .input and a .output file"
  io.puts "  --errors              Create a .errors file"
  io.puts "  --warnings            Create a .warnings file"
  io.puts "  --runtime-errors      Create a .runtime-errors file"
  io.puts "  --osl                 Create a .osl file"
end

SupportedOptions = %w(--help --interpreter --errors --warnings --runtime-errors --osl)
options = []
while !ARGV.empty? && ARGV.first.start_with?("--")
  option = ARGV.shift
  if SupportedOptions.include? option
    options << option
  else
    $stderr.puts "Unsupported option: #{option}"
    print_usage($stderr)
    exit 1
  end
end

if options.include? "--help"
  print_usage $stdout
  exit 0
end

if ARGV.length != 1
  $stderr.puts "There should be exactly one non-option argument"
  print_usage($stderr)
  exit 1
end

TestCase = ARGV.shift
TestCasePath = "src/test/resources/de/uni_luebeck/isp/tessla/interpreter/tests/#{TestCase}"

if File.exist? "#{TestCasePath}.json"
  $stderr.puts "#{TestCasePath}.json already exists, aborting."
  exit 1
end

def add_entry(json, key, extension, comma = true)
  f = "#{TestCasePath}.#{extension}"
  if File.exist? f
    puts "#{f} already exists, skipping."
  else
    File.open(f, "w") do
      puts "Created #{f}."
    end
  end
  basename = File.basename "#{TestCase}.#{extension}"
  json.print "  \"#{key}\": \"#{basename}\""
  if comma
    json.print ","
  end
  json.puts
end

File.open("#{TestCasePath}.json", "w") do |json|
  json.puts "{"
  if options.include? "--interpreter"
    add_entry json, "input", "input"
    add_entry json, "expectedOutput", "output"
  end
  if options.include? "--errors"
    add_entry json, "expectedErrors", "errors"
  end
  if options.include? "--warnings"
    add_entry json, "expectedWarnings", "warnings"
  end
  if options.include? "--runtime-errors"
    add_entry json, "expectedRuntimeErrors", "runtime-errors"
  end
  if options.include? "--osl"
    add_entry json, "expectedOsl", "osl"
  end
  add_entry json, "spec", "tessla", false
  json.puts "}"
end

puts "Created #{TestCasePath}.json."
