pub struct BufferedWriter {
	bytes_written: usize,
	buffer: Box<[u8]>,
	cursor: usize,
}

impl BufferedWriter {
	pub fn new() -> BufferedWriter {
		BufferedWriter {
			bytes_written: 0,
			buffer: Box::new([0; 32768]),
			cursor: 0,
		}
	}

	pub fn write(self: &mut BufferedWriter, byte: u8) {
		println!("Emitting byte: {}", byte as char);
		self.buffer[self.cursor] = byte;
		self.bytes_written += 1;
		self.cursor = (self.cursor + 1) % self.buffer.len();
	}

	pub fn repeat(self: &mut BufferedWriter, distance: usize) -> u8 {
		println!("Repeating -{}", distance);
		assert!(distance <= self.buffer.len());
		assert!(distance <= self.bytes_written);

		// This is expected to be `cursor - distance` but because we're
		// using a circular buffer, we need to add `capacity` and mod by
		// `capacity` to ensure a positive index when we wrap around
		let capacity = self.buffer.len();
		let index = (capacity + self.cursor - distance) % capacity;
		let byte = self.buffer[index];
		println!("Byte at index {} is {}", index, byte);
		self.write(byte);
		byte
	}
}
