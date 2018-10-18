use super::Source;
use super::huffman::Huffman;
use super::bitstream::BitStream;
use super::bufferedwriter::BufferedWriter;

struct DynamicBlock {
	// If we encounter a repeat, we should set our state so that we keep producing
	// repeats until they run out, instead of reading from the bit stream.
	repeats_remaining: usize,
	last_repeat_distance: usize,

	// Used to determine next byte from next bit(s) in stream
	literal_length_huffman: Huffman<usize>,
	distance_huffman: Huffman<usize>,
}

impl DynamicBlock {
	fn new(bit_stream: &mut BitStream) -> DynamicBlock {
		let hlit = bit_stream.next_bits(5) as usize + 257;
		let hdist = bit_stream.next_bits(5) as usize + 1;
		let hclen = bit_stream.next_bits(4) as usize + 4;
		println!("hlit : {}", hlit);
		println!("hdist : {}", hdist);
		println!("hclen : {}", hclen);

		// Read this first, this is next in the format and contains the code length
		// compression tree to decode the literal/length huffman and the distance huffman
		let code_length_huffman = DynamicBlock::read_code_length_huffman(hclen, bit_stream);

		// Use code_length_huffman to build the literal_length and distance huffmans
		let literal_length_huffman = DynamicBlock::read_literal_length_huffman(hlit, bit_stream, &code_length_huffman);
		let distance_huffman = DynamicBlock::read_distance_huffman(hdist, bit_stream, &code_length_huffman);

		DynamicBlock{
			repeats_remaining: 0,
			last_repeat_distance: 0,
			literal_length_huffman,
			distance_huffman,
		}
	}

	fn read_repeat_distance(distance_huffman: &Huffman<usize>, input_stream: &mut BitStream) -> usize {
		// A code ends up mapping to some base distance plus some
		// extra bits to read to add to that base distance
		let code = DynamicBlock::get_next_huffman_encoded_value(&distance_huffman, input_stream);
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

	fn read_code_length_huffman(length: usize, input_stream: &mut BitStream) -> Huffman<usize> {
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
		let h = Huffman::new(&alphabet, &bit_lengths);
		println!("Code Length Huffman = {:?}", h);
		h
	}

	fn read_literal_length_huffman(length: usize, input_stream: &mut BitStream, code_length_huffman: &Huffman<usize>) -> Huffman<usize> {
		// Includes 0 and 285, but not 286
		let alphabet = (0..length).collect();
		let lengths = DynamicBlock::read_code_lengths(length, input_stream, code_length_huffman);
		let result = Huffman::new(&alphabet, &lengths);
		println!("Literal/Length Huffman = {:?}", result);
		result
	}

	fn read_distance_huffman(length: usize, input_stream: &mut BitStream, code_length_huffman: &Huffman<usize>) -> Huffman<usize> {
		let alphabet = (0..length).collect();
		let lengths = DynamicBlock::read_code_lengths(length, input_stream, code_length_huffman);
		let result = Huffman::new(&alphabet, &lengths);
		println!("Distance Huffman = {:?}", result);
		result
	}

	fn read_code_lengths(count: usize, input_stream: &mut BitStream, code_length_huffman: &Huffman<usize>) -> Vec<usize> {
		let mut lengths = Vec::new();
		while lengths.len() < count {
			let length = DynamicBlock::get_next_huffman_encoded_value(code_length_huffman, input_stream);
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

	fn get_next_huffman_encoded_value<T : Copy + Eq>(huffman: &Huffman<T>, input_stream: &mut BitStream) -> T {
		match huffman {
			Huffman::Branch{zero, one} => {
				if input_stream.next() {
					DynamicBlock::get_next_huffman_encoded_value(one, input_stream)
				} else {
					DynamicBlock::get_next_huffman_encoded_value(zero, input_stream)
				}
			},
			Huffman::Leaf(value) => *value,
			Huffman::DeadEnd => panic!("Reached dead end!"),
		}
	}

	fn next(&mut self, bit_stream: &mut BitStream) -> DEFLATEResult {
		if self.repeats_remaining > 0 {
			self.repeats_remaining -= 1;
			return DEFLATEResult::Repeat(self.last_repeat_distance)
		}

		let value = DynamicBlock::get_next_huffman_encoded_value(&self.literal_length_huffman, bit_stream);
		println!("Found value: {}", value);
		if value < 256 {
			// This is a literal byte to emit to the output stream
			// We know it's a byte because of the check above and
			// it's defined that way by the standard
			DEFLATEResult::Literal(value as u8)
		} else if value == 256 {
			println!("End of block encountered");
			DEFLATEResult::EndOfBlock
		} else if value <= 285 {
			// The value is between [257, 285] inclusive on both ends
			// This means it's a back reference so we have to copy
			// from the buffer of written bytes some distance away
			// and for some amount of repetition

			let repeat_length = DynamicBlock::read_repeat_length(value, bit_stream);
			let distance = DynamicBlock::read_repeat_distance(&self.distance_huffman, bit_stream);
			self.last_repeat_distance = distance;
			self.repeats_remaining = repeat_length - 1;
			DEFLATEResult::Repeat(distance)
		} else {
			panic!("Unsupported value: {}", value);
		}
	}
}

enum DEFLATEResult {
	Literal(u8),
	EndOfBlock,
	Repeat(usize),
}

/// Keeps track of the state of the deflater.  The state is necessary because
/// although we emit one byte at a time, we can generate multiple bytes at a
/// time with the repeat function.
pub struct DEFLATEReader<'a> {
	// The following two fields are used to manage input/output
	buffered_writer: BufferedWriter,
	bit_stream: BitStream<'a>,

	// The following two fields control if we read another block
	has_seen_final_block: bool,
	current_block: Option<DynamicBlock>,
}

const BTYPE_DYNAMIC : u8 = 0b10;

impl <'a> DEFLATEReader<'a> {
	pub fn new<T : Source<u8> + 'a>(input: &'a mut T) -> DEFLATEReader<'a> {
		DEFLATEReader{
			buffered_writer: BufferedWriter::new(),
			bit_stream: BitStream::new(input),
			has_seen_final_block: false,
			current_block: None,
		}
	}
}

impl <'a> Source<Option<u8>> for DEFLATEReader<'a> {
	fn next(self: &mut DEFLATEReader<'a>) -> Option<u8> {
		// We set this field when we have finished with a block or haven't started yet
		// this field tells us that we should begin reading a new block which involves
		// decoding all the headers and huffman trees
		if self.current_block.is_none() {
			// We have already fully consumed the final block
			if self.has_seen_final_block {
				return None
			}

			// Is this the final block?
			let b_final = self.bit_stream.next();
			println!("b_final: {}", b_final);
			self.has_seen_final_block = b_final;

			// Is what type of block is this?
			let b_type = self.bit_stream.next_bits(2) as u8;
			println!("b_type : {:02b}", b_type);
			self.current_block = match b_type {
				BTYPE_DYNAMIC => Some(DynamicBlock::new(&mut self.bit_stream)),
				_ => panic!("Unhandled block type: {:02b}", b_type),
			};
		}

		// The above actions should have established a current block if one was needed
		assert!(self.current_block.is_some());

		match self.current_block.as_mut() {
			Some(block) => match block.next(&mut self.bit_stream) {
				DEFLATEResult::Literal(byte) => {
					println!("Is Literal..");
					self.buffered_writer.write(byte);
					Some(byte)
				},
				DEFLATEResult::EndOfBlock => {
					println!("Is EOB..");
					self.current_block = None;
					let byte = self.next();
					println!("Continuing EOB");
					// We probably don't need the following because
					// the other branches handle writing already
					// if byte.is_some() {
					// 	self.buffered_writer.write(byte.unwrap());
					// }
					byte
				},
				DEFLATEResult::Repeat(distance) => {
					println!("Is Repeat..");
					Some(self.buffered_writer.repeat(distance))
				},
			},
			None => panic!("We should have assured not none above"),
		}
	}
}
