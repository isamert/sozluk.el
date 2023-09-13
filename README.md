# sozluk.el
An online Turkish dictionary that uses `sozluk.tdk.gov.tr` and `etimolojiturkce.com`.

## Installation

sozluk is available through [MELPA](https://melpa.org/#/sozluk). If you have it set up already, just do `M-x package-install sozluk` and you are good to go. Otherwise please see [MELPA getting started](https://melpa.org/#/getting-started) page to learn how you can install packages through MELPA or see the following installation options.

Another way to install `sozluk.el` would be using either [straight](https://github.com/radian-software/straight.el) or [quelpa](https://github.com/quelpa/quelpa-use-package) package managers:

```elisp
(use-package sozluk
  :quelpa (sozluk :fetcher github :repo "isamert/sozluk.el"))
```

Alternatively, you can use the straight package manager:

```elisp
(use-package sozluk
  :straight (sozluk :type git :host github :repo "isamert/sozluk.el"))
```

Another option is just downloading sozluk.el file and putting into your load-path, afterwards you can simply do the following in your init.el:

```elisp
(require 'sozluk)
```


## Demo
### `sozluk`

![sozluk_sozluk](https://user-images.githubusercontent.com/8031017/156441505-3499a77e-aa8a-448a-ad70-94e1ab1697af.gif)

### `sozluk-etymology`

![sozluk_etymology](https://user-images.githubusercontent.com/8031017/156441500-6cac468a-dd63-44b3-8879-4ba4e79f20f3.gif)
