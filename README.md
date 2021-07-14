# DS304.L21 - Thiết kế và Phân tích thực nghiệm

## Thông tin môn học
* **Môn học:** Thiết kế và Phân tích thực nghiệm
* **Lớp:** DS304.L21
* **Năm học:** HKII NH 2020-2021
* **Giảng viên:** TS. Đỗ Trọng Hợp

## Thông tin nhóm
STT | Họ tên | MSSV | Github
--- | -------|------|--------
1 | Thái Minh Triết | 19522397 | https://github.com/minhtriet2507
2 | Chu Hà Thảo Ngân | 19521882 | https://github.com/thaongan251
3 | Võ Tuấn Anh | 19521226 | https://github.com/VOTUANANH01

## Đề tài báo cáo
**Phân tích hồi quy bộ dữ liệu Diabetes**

## Source Code

## Tài liệu tham khảo
For Codebook: https://scikit-learn.org/stable/datasets/toy_dataset.html#diabetes-dataset

For Preprocessing: https://towardsdatascience.com/data-preparation-and-preprocessing-is-just-as-important-creating-the-actual-model-in-data-sciences-2c0562b65f62

##Latest Notes:
Nhóm thống nhất dùng hàm này để chia train test :

```
set.seed(123)
train.index <- createDataPartition(df$Y, p = .8, list = FALSE)
train <- df[ train.index,]
test  <- df[-train.index,]
```
