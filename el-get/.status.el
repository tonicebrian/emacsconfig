((el-get status "required" recipe
	 (:name el-get :type git :url "https://github.com/dimitri/el-get.git" :features el-get :load "el-get.el" :compile "el-get.el"))
 ("el-get" status "installed" recipe
  (:name el-get :type git :url "https://github.com/dimitri/el-get.git" :features el-get :load "el-get.el" :compile "el-get.el"))
 (rect-mark status "installed" recipe
	    (:name rect-mark :type emacswiki))
 (yasnippet status "required" recipe
	    (:name yasnippet :type svn :url "http://yasnippet.googlecode.com/svn/trunk/" :features "yasnippet" :post-init
		   (lambda nil
		     (yas/initialize)
		     (add-to-list 'yas/snippet-dirs
				  (concat el-get-dir "yasnippet/snippets"))
		     (yas/reload-all)))))
