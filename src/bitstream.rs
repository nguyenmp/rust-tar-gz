pub struct BitStream<'a> {
	current_byte : u8,
	current_position : usize,
	reader : &'a mut super::Source<u8>,
}

impl<'a> BitStream<'a> {
	pub fn new(input: &'a mut super::Source<u8>) -> BitStream<'a> {
		BitStream{
			current_byte: input.next(),
			current_position: 0,
			reader: input,
		}
	}

	pub fn next_bits(self: &mut BitStream<'a>, num_bits: usize) -> u16 {
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
			result |= (self.next() as u16) << bit_position
		}
		result
	}

	pub fn next(self: &mut BitStream<'a>) -> bool {
		// We cannot read past the 7th bit in a byte (indexed by zero)
		assert!(self.current_position <= 7);

		// This object functiosn by having hte cursor set on the next bit
		// So to get the next bit, we just access it
		let bit = (self.current_byte >> self.current_position) & 0b00000001;

		// After accessing it, we need to move the cursor
		// up or wrap to the next byte in the input stream
		if self.current_position == 7 {
			self.current_position = 0;
			self.current_byte = self.reader.next();
		} else {
			self.current_position += 1;
		}

		// There should be no other bits other than the least significant
		assert_eq!(bit & !0b1, 0);
		bit == 0b1
	}
}
