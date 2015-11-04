


	$(function(){
	 $('.a-box .a2').click(function(){
	         $('.pop-body').show()
	   })

	   $('.a-box .a2').blur(function(){
	         $('.pop-body').hide()
	   })
	})

    //检查油箱
    var booleanEamil = true;
    function checkEmail(){
    	booleanEamil = true;
     	var email =  document.getElementById("email").value.trim();
    	var check = /^([0-9A-Za-z\-_\.]+)@([0-9a-z]+\.[a-z]{2,3}(\.[a-z]{2})?)$/g;
    	if(email!=null&&email!=""&&!check.test(email)){
    		document.getElementById("errorMsg").innerText="账户不符合邮件规则";
    		booleanEamil = false;
 			return false;
    	}
     }
    
    //校验昵称
    var booleanUserName = true;
    function checkLength() {  
        var userName = document.getElementById("nickname").value;
        if(userName.replace(/[^\x00-\xff]/g,"**").length>20){
    		document.getElementById("errorMsg").innerText="昵称最多输入10个汉字";
    		booleanUserName = false;
        }else{
        	booleanUserName = true;
        }
    }
    
    //检验日期
    var booleanBirthday = true;
    function checkBirthday(){
    	var check = /^((((19|20)\d{2})-(0?(1|[3-9])|1[012])-(0?[1-9]|[12]\d|30))|(((19|20)\d{2})-(0?[13578]|1[02])-31)|(((19|20)\d{2})-0?2-(0?[1-9]|1\d|2[0-8]))|((((19|20)([13579][26]|[2468][048]|0[48]))|(2000))-0?2-29))$/;
    	var birthday = document.getElementById("myDate").value;
    	if(check.test(birthday)==false){
	   		document.getElementById("errorMsg").innerText="输入的生日不符合规则";
	   		booleanBirthday = false;
		 }else{
			booleanBirthday = true;
		 }
    }
    
    function onChange(){
    	document.getElementById("errorMsg").innerText="";
    }
    
    //提交表单
    function submitForm(){
    	checkEmail();
    	checkLength();
    	checkBirthday();
 	   if(!booleanUserName||!booleanBirthday||!booleanEamil){
 		   return false;
 	   }
 	   document.getElementById("changeForm").submit();
 	}
    
    //图片预览
    function handleFiles(obj) {
   	   	var fileList = document.getElementById("photoUrl");
   		window.URL = window.URL || window.webkitURL;
   		var files = obj.files,
   			img = new Image();
   		if(window.URL){
   		      img.src = window.URL.createObjectURL(files[0]); //创建一个object URL，并不是你的本地路径
   		}else if(window.FileReader){
   			var reader = new FileReader();
   			reader.readAsDataURL(files[0]);
   			reader.onload = function(e){
   			img.src = this.result;
   			}
   		}else{
   			obj.select();
   			obj.blur();
   			var nfile = document.selection.createRange().text;
   			document.selection.empty();
   			img.src = nfile;
   		}
   		fileList.src = img.src;
   		document.getElementById("iptFile").value = document.getElementById("inputfile").value;
   	}

