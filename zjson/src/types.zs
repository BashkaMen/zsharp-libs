type JsonWriter =
    { buf: ref<Buffer<u8>>
      len: ref<int>
      needComma: ref<bool> }
