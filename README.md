# sozluk.el
An online Turkish dictionary that uses `sozluk.tdk.gov.tr` and `etimolojiturkce.com`.

## Installation

You can install sozluk.el using `quelpa`:

```elisp
(use-package sozluk
  :ensure t
  :quelpa (sozluk
           :fetcher github
           :repo "isamert/sozluk.el"))
```

Alternatively, you can use the straight package manager:

```elisp
(use-package sozluk
  :ensure t
  :straight (sozluk
	         :type git
	         :host github
	         :repo "isamert/sozluk.el"))
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
