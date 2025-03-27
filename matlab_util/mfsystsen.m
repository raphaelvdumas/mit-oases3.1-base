function sen = mfsystsen(pre, vga, eq, f)

sen = -178+pre+vga+10*log10((f/853).^2./(1+(f/853).^2))+...
      10*log10((f/eq).^2./(1+(f/eq).^2));
