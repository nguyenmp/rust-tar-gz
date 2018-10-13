#[macro_use]
extern crate serde_derive;

extern crate serde;
extern crate serde_json;

use std::fs::File;

fn main() {
	let filename = "/Users/livingon/Downloads/sysdiagnose_2018.10.03_00-15-12-0700_Mac_OS_X_BXPCFACP0-1_18A326g.tar.gz";
	let mut file = File::open(filename).expect("file not found");
	gzip::handle_headers(&mut file);
	deflate::process(&mut file);
}

pub mod huffman {
	#[derive(Eq, PartialEq, Serialize)]
	pub enum Huffman<T : Copy + Eq> {
		Branch{
			zero: Box<Huffman<T>>,
			one: Box<Huffman<T>>,
		},
		Leaf(T),
		DeadEnd,
	}

	impl <T: Copy + Eq + serde::ser::Serialize> std::fmt::Debug for Huffman<T> {
	    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
	        write!(f, "{}", serde_json::to_string(&self).unwrap())
	    }
	}


	impl <T : Copy + Eq + Ord> Huffman<T> {
		pub fn new(alphabet: &Vec<T>, bit_lengths: &Vec<usize>) -> Huffman<T> {
			assert_eq!(alphabet.len(), bit_lengths.len());

			let counts_by_length = count_by_code_length(bit_lengths);
			println!("Counts By Length: {:?}", counts_by_length);
			let max = get_max_code_length(bit_lengths);
			let starting_values = determine_starting_values(&counts_by_length, max);
			println!("Starting Values: {:?}", starting_values);
			let assigned_values = assign_values(alphabet, bit_lengths, &starting_values, max);
			println!("Assigned Values: {:?}", assigned_values);
			build_tree(alphabet, &assigned_values, bit_lengths)
		}
	}

	fn count_by_code_length(bit_lengths: &Vec<usize>) -> Vec<usize> {
		// Since we are counting the number of codes that have the same code length
		// The result only needs enough space to store up to the maximum code length
		// The value of this result map will be the number of codes that have that
		// the input length, which is the key/index to this array
		let max_code_length = get_max_code_length(bit_lengths);
		let mut count_by_code_length = vec![0; max_code_length + 1];
		for bit_length in bit_lengths {
			count_by_code_length[*bit_length] += 1;
		}
		count_by_code_length
	}

	fn get_max_code_length(bit_lengths: &Vec<usize>) -> usize {
		let mut max = 0;
		for bit_length in bit_lengths {
			if *bit_length > max {
				max = *bit_length;
			}
		}
		max
	}

	fn determine_starting_values(count_by_code_length: &Vec<usize>, max_code_length: usize) -> Vec<usize> {
		// We always start with an array of 0 because we start counting at length=1
		let mut starting_values = Vec::new();
		starting_values.push(0);

		for length in 1..count_by_code_length.len() {
			let last_value = starting_values[length - 1];
			let last_count = if length == 1 { 0 } else { count_by_code_length[length - 1] };
			starting_values.push((last_value + last_count) << 1);
			println!("Previous Value: {}, Previous Count: {}, New Value: {}", last_value, last_count, starting_values.last().unwrap());
		}
		starting_values
	}

	fn assign_values<T : Copy + Ord>(alphabet: &Vec<T>, bit_lengths: &Vec<usize>, starting_values: &Vec<usize>, max_length: usize) -> Vec<usize> {
		println!("Assigning real values given these lengths: {:?} and starting_values: {:?}", bit_lengths, starting_values);
		let mut values = vec![0; bit_lengths.len()];
		let index_order = get_index_ordering(alphabet);


		// We need max_length + 1 because we're using that value as an index
		// into an array.  Otherwise, we'd never actually look at max_length
		// since ranges are not inclusive on that end
		for current_length in 1..(max_length + 1) {
			let mut starting_value = starting_values[current_length];
			for index in &index_order {
				if bit_lengths[*index] == current_length {
					values[*index] = starting_value;
					starting_value += 1;
				}
			}
		}
		values
	}

	/// This inverts the code length huffman tree's alphabet
	/// Huffman trees work by assigning value to the lowest lexical
	/// value first, but the alphabet is not in lexical order
	/// So we need to figure out how to invert the alphabet to get
	/// the order of indexes to walk through
	fn get_index_ordering<T : Ord + Copy>(alphabet: &Vec<T>) -> Vec<usize> {
		// alphabet is givne_index -> value
		// sorted is walking_index -> value
		// result is walking_index -> given_index
		let mut sorted = alphabet.clone();
		sorted.sort_unstable();
		let mut result = vec![0; alphabet.len()];
		for given_index in 0..alphabet.len() {
			let value = alphabet[given_index];
			let walking_index = sorted.binary_search(&value).expect("Alphabet inversion resulted in not finding a value");
			result[walking_index] = given_index;
		}
		println!("Walking order: {:?}", result);
		result
	}

	fn build_tree<T : Copy + Eq>(alphabet: &Vec<T>, values: &Vec<usize>, lengths: &Vec<usize>) -> Huffman<T> {
		let depth = 0;
		let mut indexes = Vec::new();
		for index in 0..lengths.len() {
			let length = lengths[index];
			if length > 0 {
				indexes.push(index)
			}
		}
		println!("Only considering {:?}", indexes);
		_build_tree(alphabet, values, depth, lengths, &indexes)
	}

	fn _build_tree<T : Copy + Eq>(alphabet: &Vec<T>, values: &Vec<usize>, depth: usize, lengths: &Vec<usize>, indexes: &Vec<usize>) -> Huffman<T> {
		// If we only have one index to consider, then we are at the leaf
		// and the value of that leaf is the alphabet at that index
		if indexes.len() == 1 {
			println!("Leafing this alone!");
			return Huffman::Leaf(alphabet[indexes[0].clone()])
		} else if indexes.len() == 0 {
			println!("This is a dead end, and that's okay");
			return Huffman::DeadEnd
		}

		// Otherwise, we need to loop through our options and
		// descriminate between the ones and zeros
		// Group the considerations by the current bit value = one or = zero
		// this will parititon the alphabet into the left and right branches of the tree
		let mut zeros = Vec::new();
		let mut ones = Vec::new();
		for index in indexes {
			let length = lengths[*index];
			let value = values[*index];

			// If we're at depth 0, and we're looking at a value of length 3
			// Then the bit we are looking at is the 2 in [2, 1, 0]
			let bit = (value >> (length - 1 - depth)) & 0b1;
			println!("Index {:?}, Value {:06b}, length {}, depth {}, bit {:01b}", *index, value, length, depth, bit);
			if bit == 1 {
				ones.push(*index);
			} else if bit == 0 {
				zeros.push(*index);
			} else {
				panic!("bit was neither one nor zero");
			}
		}

		println!("Ones: {:?}, Zeros: {:?}", ones, zeros);

		Huffman::Branch {
			zero: Box::new(_build_tree(alphabet, values, depth + 1, lengths, &zeros)),
			one: Box::new(_build_tree(alphabet, values, depth + 1, lengths, &ones)),
		}
	}

	#[cfg(test)]
	mod tests {
		use super::*;
		#[test]
		fn create_huffman() {
			let alphabet = vec!['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H'];
			let bit_lengths = vec![3, 3, 3, 3, 3, 2, 4, 4];
			let tree = Huffman::new(&alphabet, &bit_lengths);
			assert_eq!(
				tree,
				Huffman::Branch {
					zero: Box::new(Huffman::Branch {
						zero: Box::new(Huffman::Leaf('F')),
						one: Box::new(Huffman::Branch {
							zero: Box::new(Huffman::Leaf('A')),
							one: Box::new(Huffman::Leaf('B'))
						})
					}),
					one: Box::new(Huffman::Branch {
						zero: Box::new(Huffman::Branch {
							zero: Box::new(Huffman::Leaf('C')),
							one: Box::new(Huffman::Leaf('D'))
						}),
						one: Box::new(Huffman::Branch {
							zero: Box::new(Huffman::Leaf('E')),
							one: Box::new(Huffman::Branch {
								zero: Box::new(Huffman::Leaf('G')),
								one: Box::new(Huffman::Leaf('H'))
							})
						})
					})
				}
			)
		}

		#[test]
		fn test_determine_starting_values() {
			let bit_lengths = vec![3, 3, 3, 3, 3, 2, 4, 4];
			let counts_by_code_length = count_by_code_length(&bit_lengths);
			let max = get_max_code_length(&bit_lengths);
			let starting_values = determine_starting_values(&counts_by_code_length, max);
			assert_eq!(starting_values, vec![0, 0, 0, 2, 14]);
		}

		#[test]
		fn test_assign_values() {
			let alphabet = vec!['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H'];
			let bit_lengths = vec![3, 3, 3, 3, 3, 2, 4, 4];
			let counts_by_code_length = count_by_code_length(&bit_lengths);
			let max = get_max_code_length(&bit_lengths);
			let starting_values = determine_starting_values(&counts_by_code_length, max);
			let assigned_values = assign_values(&alphabet, &bit_lengths, &starting_values, max);
			assert_eq!(assigned_values, vec![0b010, 0b011, 0b100, 0b101, 0b110, 0b00, 0b1110, 0b1111]);
		}

		#[test]
		fn test_count_by_code_length() {
			let bit_lengths = vec![3, 3, 3, 3, 3, 2, 4, 4];
			let result = count_by_code_length(&bit_lengths);
			assert_eq!(result, vec![0, 0, 1, 5, 2]);
		}
	}
}


mod deflate {
	use std::io::Read;

	struct BufferedWriter {
		bytes_written: usize,
		buffer: Box<[u8]>,
		cursor: usize,
	}

	impl BufferedWriter {
		fn new() -> BufferedWriter {
			BufferedWriter {
				bytes_written: 0,
				buffer: Box::new([0; 32768]),
				cursor: 0,
			}
		}

		fn write(self: &mut BufferedWriter, byte: u8) {
			println!("Emitting byte: {}", byte as char);
			self.buffer[self.cursor] = byte;
			self.bytes_written += 1;
			self.cursor = (self.cursor + 1) % self.buffer.len();
		}

		fn repeat(self: &mut BufferedWriter, distance: usize, count: usize) {
			println!("Repeating -{} for {} times", distance, count);
			assert!(distance <= self.buffer.len());
			assert!(distance <= self.bytes_written);
			for _ in 0..count {
				// This is expected to be `cursor - distance` but because we're
				// using a circular buffer, we need to add `capacity` and mod by
				// `capacity` to ensure a positive index when we wrap around
				let capacity = self.buffer.len();
				let index = (capacity + self.cursor - distance) % capacity;
				let byte = self.buffer[index];
				self.write(byte);
			}
		}
	}

	const BTYPE_DYNAMIC : u8 = 0b10;

	pub fn process(reader: &mut Read) {
		let mut bit_stream = BitStream::from_read(reader);
		let mut writer = BufferedWriter::new();

		loop {
			let is_final_block = handle_block(&mut bit_stream, &mut writer);
			if is_final_block {
				println!("Final block has been handled, terminating DEFLATE stream");
				break
			}
		}
	}

	fn handle_block(input_stream: &mut BitStream, writer: &mut BufferedWriter) -> bool {
		// Is this the final block?
		let b_final = input_stream.next_bit();
		println!("b_final: {}", b_final);

		// Is what type of block is this?
		let b_type = input_stream.next_bits(2) as u8;
		println!("b_type : {:02b}", b_type);
		match b_type {
			BTYPE_DYNAMIC => handle_dynamic(input_stream, writer),
			_ => panic!("Unhandled block type: {:02b}", b_type),
		}
		b_final == 0b1
	}

	fn handle_dynamic(input_stream: &mut BitStream, writer: &mut BufferedWriter) {
		let hlit = input_stream.next_bits(5) as usize + 257;
		let hdist = input_stream.next_bits(5) as usize + 1;
		let hclen = input_stream.next_bits(4) as usize + 4;
		println!("hlit : {}", hlit);
		println!("hdist : {}", hdist);
		println!("hclen : {}", hclen);

		// Read this first, this is next in the format and contains the code length
		// compression tree to decode the literal/length huffman and the distance huffman
		let code_length_huffman = read_code_length_huffman(hclen, input_stream);

		// Use code_length_huffman to build the literal_length and distance huffmans
		let literal_length_huffman = read_literal_length_huffman(hlit, input_stream, &code_length_huffman);
		let distance_huffman = read_distance_huffman(hdist, input_stream, &code_length_huffman);

		// Actual decompression of data
		loop {
			let value = get_next_huffman_encoded_value(&literal_length_huffman, input_stream);
			println!("Found value: {}", value);
			if value < 256 {
				// This is a literal byte to emit to the output stream
				// We know it's a byte because of the check above and
				// it's defined that way by the standard
				writer.write(value as u8);
			} else if value == 256 {
				println!("End of block encountered");
				break
			} else if value <= 285 {
				// The value is between [257, 285] inclusive on both ends
				// This means it's a back reference so we have to copy
				// from the buffer of written bytes some distance away
				// and for some amount of repetition

				let repeat_length = read_repeat_length(value, input_stream);
				let distance = read_repeat_distance(&distance_huffman, input_stream);
				writer.repeat(distance as usize, repeat_length);
			} else {
				panic!("Unsupported value: {}", value);
			}
		}
	}

	fn read_repeat_distance(distance_huffman: &DistanceHuffman, input_stream: &mut BitStream) -> usize {
		// A code ends up mapping to some base distance plus some
		// extra bits to read to add to that base distance
		let code = get_next_huffman_encoded_value(&distance_huffman, input_stream);
		println!("Modulo: {}", code % 2);
		let base_distance = match code {
			0 ... 3 => {
				code as u32 + 1
			},
			_ if code % 2 == 0 => {
				println!("Even code");
				2u32.pow(code as u32 / 2) + 1
			},
			_ if code % 2 == 1 => {
				println!("Odd code");
				println!("{}", 2u32.pow(code as u32 / 2));
				println!("{}", 2u32.pow(code as u32 / 2 - 1));
				println!("{}", 2u32.pow(code as u32 / 2) + 2u32.pow(code as u32 / 2 - 1) + 1);
				2u32.pow(code as u32 / 2) + 2u32.pow(code as u32 / 2 - 1) + 1
			},
			_ => panic!("Logic error handling base distance"),
		};
		let num_distance_extra_bits = match code {
			0 ... 3 => 0,
			4 ... 29 => (code / 2) - 1,
			_ => panic!("Distance is undefined for: {}", code),
		};
		let distance_offset = input_stream.next_bits(num_distance_extra_bits) as u32;
		println!("Code: {} Base Distance: {} Offset: {} Bits: {}", code, base_distance, distance_offset, num_distance_extra_bits);
		let distance = base_distance + distance_offset;
		distance as usize
	}

	fn read_repeat_length(value: usize, input_stream: &mut BitStream) -> usize {
		let num_length_extra_bits = match value {
			257 ... 264 => 0,
			265 ... 284 => (value - 265) / 4 + 1,
			285 => 0,
			_ => panic!("Unsupported value for length: {}", value),
		};
		let length_offset = input_stream.next_bits(num_length_extra_bits) as usize;
		let base_length = match value {
			257 ... 264 => value - 254,
			265 ... 268 => 11 + 2 * (value - 265),
			269 ... 272 => 19 + 4 * (value - 269),
			273 ... 276 => 35 + 8 * (value - 273),
			277 ... 280 => 67 + 16 * (value - 277),
			281 ... 284 => 131 + 32 * (value - 281),
			285 => 258,
			_ => panic!("Unsupported value for length: {}", value),
		};
		println!("Base Length: {} Offset: {} Bits: {}", base_length, length_offset, num_length_extra_bits);
		let length = base_length + length_offset;
		return length
	}

	type CodeLengthHuffman = super::huffman::Huffman<usize>;
	type LiteralLengthHuffman = super::huffman::Huffman<usize>;
	type DistanceHuffman = super::huffman::Huffman<usize>;
	fn read_code_length_huffman(length: usize, input_stream: &mut BitStream) -> CodeLengthHuffman {
		// Read the hlit + 4 code lengths (3 bits each)
		let alphabet = vec![16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15];
		let mut bit_lengths = vec![0; alphabet.len()];
		assert_eq!(alphabet.len(), 19);
		for index in 0..length {
			let length = input_stream.next_bits(3) as usize;
			bit_lengths[index] = length
		}
		println!("Alphabet : {:?}", alphabet);
		println!("Bit Lengths : {:?}", bit_lengths);
		let h = super::huffman::Huffman::new(&alphabet, &bit_lengths);
		println!("Code Length Huffman = {:?}", h);
		h
	}

	fn read_literal_length_huffman(length: usize, input_stream: &mut BitStream, code_length_huffman: &CodeLengthHuffman) -> LiteralLengthHuffman {
		// Includes 0 and 285, but not 286
		let alphabet = (0..length).collect();
		let lengths = read_code_lengths(length, input_stream, code_length_huffman);
		let result = super::huffman::Huffman::new(&alphabet, &lengths);
		println!("Literal/Length Huffman = {:?}", result);
		result
	}

	fn read_distance_huffman(length: usize, input_stream: &mut BitStream, code_length_huffman: &CodeLengthHuffman) -> DistanceHuffman {
		let alphabet = (0..length).collect();
		let lengths = read_code_lengths(length, input_stream, code_length_huffman);
		let result = super::huffman::Huffman::new(&alphabet, &lengths);
		println!("Distance Huffman = {:?}", result);
		result
	}

	fn read_code_lengths(count: usize, input_stream: &mut BitStream, code_length_huffman: &CodeLengthHuffman) -> Vec<usize> {
		let mut lengths = Vec::new();
		while lengths.len() < count {
			let length = get_next_huffman_encoded_value(code_length_huffman, input_stream);
			println!("Found this length: {}", length);

			// Literal value
			if length <= 15 {
				lengths.push(length);
				continue
			}
			
			// Otherwise, it's a repeater of a previous value or zero
			let (repeat_value, count) = match length {
				16 => {
					let value = (*lengths.last().expect("Cannot repeat at start of stream")).clone();
					let count = input_stream.next_bits(2) + 3;
					(value, count)
				},
				17 => (0, input_stream.next_bits(3) + 3),
				18 => (0, input_stream.next_bits(7) + 11),
				_ => panic!("Unsupported code length {}", length)
			};
			for _ in 0..count {
				lengths.push(repeat_value)
			}
		}

		// By the end, we should NOT have more or less than we want
		// The encoding should generate exactly `count` entries into
		// the list of code lengths
		assert_eq!(lengths.len(), count);
		println!("Lengths by alphabet: {:?}", lengths);
		lengths
	}

	fn get_next_huffman_encoded_value<T : Copy + Eq>(huffman: &super::huffman::Huffman<T>, input_stream: &mut BitStream) -> T {
		match huffman {
			super::huffman::Huffman::Branch{zero, one} => {
				let bit = input_stream.next_bit();
				if bit == 0b0 {
					get_next_huffman_encoded_value(zero, input_stream)
				} else {
					get_next_huffman_encoded_value(one, input_stream)
				}
			},
			super::huffman::Huffman::Leaf(value) => *value,
			super::huffman::Huffman::DeadEnd => panic!("Reached dead end!"),
		}
	}

	struct BitStream<'a> {
		current_byte : u8,
		current_position : u8,
		reader : &'a mut Read ,
	}

	impl<'a> BitStream<'a> {
		fn from_read(input: &mut Read) -> BitStream {
			BitStream{
				current_byte: super::utils::get_byte(input),
				current_position: 0,
				reader: input,
			}
		}

		fn next_bits(self: &mut BitStream<'a>, num_bits: usize) -> u16 {
			// We must assert this because that's our return size
			// It's physically impossible for us to return more
			// than 16 bits in a u16
			assert!(num_bits <= 16);

			// I think given the spec of DEFLATE, we will never need to read
			// more than 13 bits at a time.  I'm adding this assert so that
			// if I ever believe I need more bits, I should double-check
			assert!(num_bits <= 13);

			let mut result : u16 = 0b0;
			for bit_position in 0..num_bits {
				result |= (self.next_bit() as u16) << bit_position
			}
			result
		}

		fn next_bit(self: &mut BitStream<'a>) -> u8 {
			// We cannot read past the 7th bit in a byte (indexed by zero)
			assert!(self.current_position <= 7);

			// This object functiosn by having hte cursor set on the next bit
			// So to get the next bit, we just access it
			let bit = (self.current_byte >> self.current_position) & 0b00000001;

			// After accessing it, we need to move the cursor
			// up or wrap to the next byte in the input stream
			if self.current_position == 7 {
				self.current_position = 0;
				self.current_byte = super::utils::get_byte(self.reader);
			} else {
				self.current_position += 1;
			}

			bit
		}
	}
}

mod utils {
	use std::io::Read;

	pub fn get_byte(input: &mut Read) -> u8 {
		let mut bytes : Vec<u8> = vec![0; 1];
		match input.read(&mut bytes[..]) {
			Ok(1) => {
				let byte = bytes[0];
				println!("Successfully read {:02x?}", byte);
				byte
			},
			Ok(bytes_read) => panic!("Only read {} bytes", bytes_read),
			Err(err) => panic!("Error: {}", err),
		}
	}

	pub fn fill_bytes(input: &mut Read, buffer: &mut [u8]) -> usize {
		match input.read(buffer) {
			Ok(bytes_read) => {
				println!("Successfully read {} bytes", bytes_read);
				bytes_read
			},
			Err(err) => panic!("Error: {}", err),
		}
	}
}

mod gzip {
	use std::io::Read;

	/// The constant value of CM (Compression Method) header field as defined in
	/// RFC 1952.  This is the only compression method supported.
	const CM_DEFLATE : u8 = 8;

	/// Bit mask to extract or set the "text" flag
	const _FLAG_TEXT : u8 = 0b00000001;

	/// Bit mask to extract or set the "HCRC" flag
	const FLAG_HCRC : u8 = 0b00000010;

	/// Bit mask to extract or set the "extra" flag
	const FLAG_EXTRA : u8 = 0b00000100;

	/// Bit mask to extract or set the "name" flag
	const FLAG_NAME : u8 = 0b00001000;

	/// Bit mask to extract or set the "comment" flag
	const FLAG_COMMENT : u8 = 0b00010000;

	/// Processes an input stream as a gzip compressed file
	pub fn handle_headers<T: Read>(input: &mut T) {
		let id1 = super::utils::get_byte(input);
		assert_eq!(id1, 0x1f);
		assert_eq!(id1, 0o037);
		assert_eq!(id1, 31);
		println!("ID1 is correct");

		let id2 = super::utils::get_byte(input);
		assert_eq!(id2, 0x8b);
		assert_eq!(id2, 0o213);
		assert_eq!(id2, 139);
		println!("ID2 is correct");

		let compression_method = super::utils::get_byte(input);
		assert_eq!(compression_method, CM_DEFLATE);
		println!("CM is correct");

		let _flags = super::utils::get_byte(input);
		println!("Flags: {:02x?}", _flags);

		let mut _mtime = vec![0; 4];
		super::utils::fill_bytes(input, &mut _mtime[..]);
		let _xfl = super::utils::get_byte(input);
		let _os = super::utils::get_byte(input);

		// At this point in the file, there can be extra fields.  I don't want to
		// implement that yet so I'm just going to assert that there are no extra
		// fields.
		assert_eq!(_flags & FLAG_EXTRA, 0);
		assert_eq!(_flags & FLAG_NAME, 0);
		assert_eq!(_flags & FLAG_COMMENT, 0);
		assert_eq!(_flags & FLAG_HCRC, 0);
	}
}