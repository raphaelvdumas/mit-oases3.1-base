function sen = vsystsen(pre, vga, eq, f)

sen = -178+pre+vga+10*log10((f/103).^2./(1+(f/103).^2))+...
      10*log10((f/eq).^2./(1+(f/eq).^2));
