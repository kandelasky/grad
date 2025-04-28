pub fn pack(v1: u8, v2: u8, v3: u8, v4: u8) -> u32 {
    ((v1 as u32) << 24) | ((v2 as u32) << 16) | ((v3 as u32) << 8) | v4 as u32
}

pub fn unpack(packed: u32) -> (u8, u8, u8, u8) {
    (
        ((packed >> 24) & 0xFF) as u8,
        ((packed >> 16) & 0xFF) as u8,
        ((packed >> 8) & 0xFF) as u8,
        (packed & 0xFF) as u8
    )
}

#[cfg(test)]
mod tests {
    #[test]
    fn pack() {
        let (v1, v2, v3, v4) = (0u8, 10u8, 20u8, 40u8);
        let pack = super::pack(v1, v2, v3, v4);
        let pack2 = {
            let unpack = super::unpack(pack);
            super::pack(unpack.0, unpack.1, unpack.2, unpack.3)
        };

        assert_eq!(pack, pack2)
    }
}