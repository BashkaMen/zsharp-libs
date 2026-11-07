type JsonWriter =
    { buf: ref<Buffer<u8>>
      len: ref<int>
      needComma: ref<bool> }

type JsonReader = { src: span<u8>; pos: ref<int> }
