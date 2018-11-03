# ktx-rw
(ABORTED) Haskell package for reading/writing data in the KTX (Khronos Texture) format.

I'm abandoning this because KTX is kind of a pain to work with.  The format is inherently
tied to OpenGL, which means that using it with Vulkan requires a lot of translation, which
I don't want to reinvent.  It's also a fairly vague and extensible format, and I don't
think I could ever build something that could handle *all* KTX files, since what they may
contain is subject to changes in the OpenGL spec.  Creating a libktx binding would probably
be a better approach.

However, I think I will just abandon the idea of using KTX altogether.  It's a pretty
uncommonly used format, so creating a binding is not worth my time.

I'll keep this code around because I learned some neat Haskell things in the process of writing it.
